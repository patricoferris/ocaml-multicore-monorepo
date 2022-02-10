(** Effects based parallel IO for OCaml *)

(** Reporting multiple failures at once. *)
module Exn : sig
  type with_bt = exn * Printexc.raw_backtrace

  exception Multiple of exn list
  (** Raised if multiple fibres fail, to report all the exceptions. *)

  val combine : with_bt -> with_bt -> with_bt
  (** [combine x y] returns a single exception and backtrace to use to represent two errors.
      Only one of the backtraces will be kept.
      The resulting exception is typically just [Multiple [y; x]],
      but various heuristics are used to simplify the result:
      - Combining with a {!Cancel.Cancelled} exception does nothing, as these don't need to be reported.
        The result is only [Cancelled] if there is no other exception available.
      - If [x] is a [Multiple] exception then [y] is added to it, to avoid nested [Multiple] exceptions.
      - Duplicate exceptions are removed (using physical equality of the exception). *)
end

(** Handles for removing callbacks. *)
module Hook : sig
  type t

  val remove : t -> unit
  (** [remove t] removes a previously-added hook.
      If the hook has already been removed, this does nothing. *)

  val null : t
  (** A dummy hook. Removing it does nothing. *)
end

(** {1 Concurrency primitives} *)

(** Grouping fibres and other resources. *)
module Switch : sig
  type t
  (** A switch contains a group of fibres and other resources (such as open file handles).
      Once a switch is turned off, the fibres should cancel themselves.
      A switch is created with [Switch.run fn],
      which does not return until all fibres attached to the switch have finished,
      and all attached resources have been closed.
      Each switch includes its own {!Cancel.t} context. *)

  val run : (t -> 'a) -> 'a
  (** [run fn] runs [fn] with a fresh switch (initially on).
      When [fn] exits, [run] waits for all operations registered with the switch to finish
      (it does not turn the switch off itself).
      If {!fail} is called, [run] re-raises the exception. *)

  val run_protected : (t -> 'a) -> 'a
  (** [run_protected fn] is like [run] but ignores cancellation requests from the parent context. *)

  val check : t -> unit
  (** [check t] checks that [t] is still on.
      @raise Cancelled If the switch is off. *)

  val get_error : t -> exn option
  (** [get_error t] is like [check t] except that it returns the exception instead of raising it.
      If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

  val fail : ?bt:Printexc.raw_backtrace -> t -> exn -> unit
  (** [fail t ex] adds [ex] to [t]'s set of failures and
      ensures that the switch's cancellation context is cancelled,
      to encourage all fibres to exit as soon as possible.
      It returns immediately, without waiting for the shutdown actions to complete.
      The exception will be raised later by {!run}, and [run]'s caller is responsible for handling it.
      {!Exn.combine} is used to avoid duplicate or unnecessary exceptions.
      @param bt A backtrace to attach to [ex] *)

  val on_release : t -> (unit -> unit) -> unit
  (** [on_release t fn] registers [fn] to be called once [t]'s main function has returned
      and all fibres have finished.
      If [fn] raises an exception, it is passed to {!fail}.
      Release handlers are run in LIFO order, in series.
      If you want to allow other release handlers to run concurrently, you can start the release
      operation and then call [on_release] again from within [fn] to register a function to await the result.
      This will be added to a fresh batch of handlers, run after the original set have finished.
      Note that [fn] is called within a {!Cancel.protect}, since aborting clean-up actions is usually a bad idea
      and the switch may have been cancelled by the time it runs. *)

  val on_release_cancellable : t -> (unit -> unit) -> Hook.t
  (** Like [on_release], but the handler can be removed later. *)

  val dump : t Fmt.t
  (** Dump out details of the switch's state for debugging. *)
end

module Promise : sig
  (** Promises are thread-safe and so can be shared between domains and used
      to communicate between them. *)

  type !'a t
  (** An ['a t] is a promise for a value of type ['a]. *)

  type 'a u
  (** An ['a u] is a resolver for a promise of type ['a]. *)

  val create : ?label:string -> unit -> 'a t * 'a u
  (** [create ()] is a fresh promise/resolver pair.
      The promise is initially unresolved. *)

  val await : 'a t -> 'a
  (** [await t] blocks until [t] is resolved.
      If [t] is already resolved then this returns immediately.
      If [t] is broken, it raises the exception. *)

  val await_result : 'a t -> ('a, exn) result
  (** [await_result t] is like [await t], but returns [Error ex] if [t] is broken
      instead of raising an exception.
      Note that if the [await_result] itself is cancelled then it still raises. *)

  val fulfill : 'a u -> 'a -> unit
  (** [fulfill u v] successfully resolves [u]'s promise with the value [v].
      Any threads waiting for the result will be added to the run queue. *)

  val break : 'a u -> exn -> unit
  (** [break u ex] resolves [u]'s promise with the exception [ex].
      Any threads waiting for the result will be added to the run queue. *)

  val resolve : 'a u -> ('a, exn) result -> unit
  (** [resolve t (Ok x)] is [fulfill t x] and
      [resolve t (Error ex)] is [break t ex]. *)

  val fulfilled : 'a -> 'a t
  (** [fulfilled x] is a promise that is already fulfilled with result [x]. *)

  val broken : exn -> 'a t
  (** [broken x] is a promise that is already broken with exception [ex]. *)

  val state : 'a t -> [`Unresolved | `Fulfilled of 'a | `Broken of exn]
  (** [state t] is the current state of [t].
      If the state is [`Unresolved] then it may change in future, otherwise it won't.
      If another domain has access to the resolver then the state may have already
      changed by the time this call returns. *)

  val is_resolved : 'a t -> bool
  (** [is_resolved t] is [true] iff [state t] is [Fulfilled] or [Broken]. *)

  val create_with_id : Ctf.id -> 'a t * 'a u
  (** Like [create], but the caller creates the tracing ID.
      This can be useful when implementing other primitives that use promises internally,
      to give them a different type in the trace output. *)
end

module Fibre : sig
  val both : (unit -> unit) -> (unit -> unit) -> unit
  (** [both f g] runs [f ()] and [g ()] concurrently.
      They run in a new cancellation sub-context, and
      if either raises an exception, the other is cancelled.
      [both] waits for both functions to finish even if one raises
      (it will then re-raise the original exception).
      [f] runs immediately, without switching to any other thread.
      [g] is inserted at the head of the run-queue, so it runs next even if other threads are already enqueued.
      You can get other scheduling orders by adding calls to {!yield} in various places.
      e.g. to append both fibres to the end of the run-queue, yield immediately before calling [both].
      If both fibres fail, {!Exn.combine} is used to combine the exceptions. *)

  val pair : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
  (** [pair f g] is like [both], but returns the two results. *)

  val all : (unit -> unit) list -> unit
  (** [all fs] is like [both], but for any number of fibres.
      [all []] returns immediately. *)

  val first : (unit -> 'a) -> (unit -> 'a) -> 'a
  (** [first f g] runs [f ()] and [g ()] concurrently.
      They run in a new cancellation sub-context, and when one finishes the other is cancelled.
      If one raises, the other is cancelled and the exception is reported.
      As with [both], [f] runs immediately and [g] is scheduled next, ahead of any other queued work.
      If both fibres fail, {!Exn.combine} is used to combine the exceptions
      (excluding {!Cancel.Cancelled} when cancelled). *)

  val any : (unit -> 'a) list -> 'a
  (** [any fs] is like [first], but for any number of fibres.
      [any []] just waits forever (or until cancelled). *)

  val await_cancel : unit -> 'a
  (** [await_cancel ()] waits until cancelled.
      @raise Cancel.Cancelled *)

  val fork : sw:Switch.t -> (unit -> unit) -> unit
  (** [fork ~sw fn] runs [fn ()] in a new fibre, but does not wait for it to complete.
      The new fibre is attached to [sw] (which can't finish until the fibre ends).
      The new fibre inherits [sw]'s cancellation context.
      If the fibre raises an exception, [sw] is turned off.
      If [sw] is already off then [fn] fails immediately, but the calling thread continues.
      [fn] runs immediately, without switching to any other fibre first.
      The calling fibre is placed at the head of the run queue, ahead of any previous items. *)

  val fork_sub : sw:Switch.t -> on_error:(exn -> unit) -> (Switch.t -> unit) -> unit
  (** [fork_sub ~sw ~on_error fn] is like [fork], but it creates a new sub-switch for the fibre.
      This means that you can cancel the child switch without cancelling the parent.
      This is a convenience function for running {!Switch.run} inside a {!fork}.
      @param on_error This is called if the fibre raises an exception.
                      If it raises in turn, the parent switch is turned off.
                      It is not called if the parent [sw] itself is cancelled. *)

  val fork_on_accept :
    on_handler_error:(exn -> unit) ->
    sw:Switch.t ->
    (Switch.t -> 'a) ->
    (Switch.t -> 'a -> unit) ->
    unit
  (** [fork_on_accept ~sw accept handle ~on_handler_error] creates a new sub-switch [t].
      It runs [accept t] in the current fibre and, on success, runs [handle t result] in a new fibre.
      It is useful for e.g. accepting network connections,
      where we need to provide a switch for the new client socket before we have forked,
      but then move it to a child fibre later.

      If [accept] raises an exception then the effect is the same as [Switch.run accept].
      If [handle] raises an exception, it is passed to [on_handler_error].
      If that raises in turn, the parent switch is turned off.
      [on_handler_error] is not called if the parent [sw] is itself cancelled. *)

  val fork_promise : sw:Switch.t -> (unit -> 'a) -> 'a Promise.t
  (** [fork_promise ~sw fn] schedules [fn ()] to run in a new fibre and returns a promise for its result.
      This is just a convenience wrapper around {!fork}.
      If [fn] raises an exception then the promise is broken, but [sw] is not turned off. *)

  val check : unit -> unit
  (** [check ()] checks that the fibre's context hasn't been cancelled.
      Many operations automatically check this before starting.
      @raise Cancel.Cancelled if the fibre's context has been cancelled. *)

  val yield : unit -> unit
  (** [yield ()] asks the scheduler to switch to the next runnable task.
      The current task remains runnable, but goes to the back of the queue.
      Automatically calls {!check} just before resuming. *)
end

val traceln :
  ?__POS__:string * int * int * int ->
  ('a, Format.formatter, unit, unit) format4 -> 'a
(** [traceln fmt] outputs a debug message (typically to stderr).
    Trace messages are printed by default and do not require logging to be configured first.
    The message is printed with a newline, and is flushed automatically.
    [traceln] is intended for quick debugging rather than for production code.

    Unlike most Eio operations, [traceln] will never switch to another fibre;
    if the OS is not ready to accept the message then the whole domain waits.

    It is safe to call [traceln] from multiple domains at the same time.
    Each line will be written atomically.

    Examples:
    {[
      traceln "x = %d" x;
      traceln "x = %d" x ~__POS__;   (* With location information *)
    ]}
    @param __POS__ Display [__POS__] as the location of the [traceln] call. *)

(** Commonly used standard features. This module is intended to be [open]ed. *)
module Std : sig
  module Promise = Promise
  module Fibre = Fibre
  module Switch = Switch

  val traceln :
    ?__POS__:string * int * int * int ->
    ('a, Format.formatter, unit, unit) format4 -> 'a
    (** Same as {!Eio.traceln}. *)
end

(** A counting semaphore.
    The API is based on OCaml's [Semaphore.Counting]. *)
module Semaphore : sig
  (** Semaphores are thread-safe and so can be shared between domains and used
      to synchronise between them. *)

  type t
  (** The type of counting semaphores. *)

  val make : int -> t
  (** [make n] returns a new counting semaphore, with initial value [n].
      The initial value [n] must be nonnegative.
      @raise Invalid_argument if [n < 0] *)

  val release : t -> unit
  (** [release t] increments the value of semaphore [t].
      If other fibres are waiting on [t], the one that has been waiting the longest is resumed.
      @raise Sys_error if the value of the semaphore would overflow [max_int] *)

  val acquire : t -> unit
  (** [acquire t] blocks the calling fibre until the value of semaphore [t]
      is not zero, then atomically decrements the value of [t] and returns. *)

  val get_value : t -> int
  (** [get_value t] returns the current value of semaphore [t]. *)
end

(** A stream/queue. *)
module Stream : sig
  (** Streams are thread-safe and so can be shared between domains and used
      to communicate between them. *)

  type 'a t
  (** A queue of items of type ['a]. *)

  val create : int -> 'a t
  (** [create capacity] is a new stream which can hold up to [capacity] items without blocking writers.
      If [capacity = 0] then writes block until a reader is ready. *)

  val add : 'a t -> 'a -> unit
  (** [add t item] adds [item] to [t].
      If this would take [t] over capacity, it blocks until there is space. *)

  val take : 'a t -> 'a
  (** [take t] takes the next item from the head of [t].
      If no items are available, it waits until one becomes available. *)

  val take_nonblocking : 'a t -> 'a option
  (** [take_nonblocking t] is like [Some (take t)] except that
      it returns [None] if the stream is empty rather than waiting.
      Note that if another domain may add to the stream then a [None]
      result may already be out-of-date by the time this returns. *)

  val length : 'a t -> int
  (** [length t] returns the number of items currently in [t]. *)

  val is_empty : 'a t -> bool
  (** [is_empty t] is [length t = 0]. *)
end

(** Cancelling other fibres when an exception occurs. *)
module Cancel : sig
  (** This is the low-level interface to cancellation.
      Every {!Switch} includes a cancellation context and most users will just use that API instead.

      Each domain has a tree of cancellation contexts, and every fibre is registered with one context.
      A fibre can switch to a different context (e.g. by calling {!sub}).
      When a context is cancelled, all registered fibres have their current cancellation function (if any)
      called and removed. Child contexts are cancelled too, recursively, unless marked as protected.

      Many operations also check that the current context hasn't been cancelled,
      so if a fibre is performing a non-cancellable operation it will still get cancelled soon afterwards.
      This check is typically done when starting an operation, not at the end.
      If an operation is cancelled after succeeding, but while still waiting on the run queue,
      it will still return the operation's result.
      A notable exception is {!Fibre.yield}, which checks at the end.
      You can also use {!Fibre.check} to check manually.

      Whether a fibre is cancelled through a cancellation function or by checking its context,
      it will receive a {!Cancelled} exception.
      It is possible the exception will get lost (if something catches it and forgets to re-raise).
      It is also possible to get this exception even when not cancelled, for example by awaiting
      a promise which another fibre has resolved to a cancelled exception.
      When in doubt, call {!Fibre.check ()} to find out if your fibre is really cancelled.
      Ideally this should be done any time you have caught an exception and are planning to ignore it,
      although if you forget then the next IO operation will typically abort anyway.

      Quick clean-up actions (such as releasing a mutex or deleting a temporary file) are OK,
      but operations that may block should be avoided.
      For example, a network connection should simply be closed,
      without attempting to send a goodbye message.

      The purpose of the cancellation system is to stop fibres quickly, not to report errors.
      Use {!Switch.fail} instead to record an error. *)

  type t
  (** A cancellation context. *)

  exception Cancelled of exn
  (** [Cancelled ex] indicates that the context was cancelled with exception [ex].
      It is usually not necessary to report a [Cancelled] exception to the user,
      as the original problem will be handled elsewhere. *)

  exception Cancel_hook_failed of exn list
  (** Raised by {!cancel} if any of the cancellation hooks themselves fail. *)

  val sub : (t -> 'a) -> 'a
  (** [sub fn] installs a new cancellation context [t], runs [fn t] inside it, and then restores the old context.
      If the old context is cancelled while [fn] is running then [t] is cancelled too.
      [t] cannot be used after [sub] returns. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect fn] runs [fn] in a new cancellation context that isn't cancelled when its parent is.
      This can be used to clean up resources on cancellation.
      However, it is usually better to use {!Switch.on_release} (which calls this for you).
      Note that [protect] does not check its parent context when it finishes. *)

  val check : t -> unit
  (** [check t] checks that [t] hasn't been cancelled.
      @raise Cancelled If the context has been cancelled. *)

  val get_error : t -> exn option
  (** [get_error t] is like [check t] except that it returns the exception instead of raising it.
      If [t] is finished, this returns (rather than raising) the [Invalid_argument] exception too. *)

  val cancel : t -> exn -> unit
  (** [cancel t ex] marks [t] and its child contexts as cancelled, recursively,
      and calls all registered fibres' cancellation functions, passing [Cancelled ex] as the argument.
      All cancellation functions are run, even if some of them raise exceptions.
      If [t] is already cancelled then this does nothing.
      Note that the caller of this function is still responsible for handling the error somehow
      (e.g. reporting it to the user); it does not become the responsibility of the cancelled thread(s).
      @raise Cancel_hook_failed if one or more hooks fail. *)

  val dump : t Fmt.t
  (** Show the cancellation sub-tree rooted at [t], for debugging. *)
end

(** {1 Cross-platform OS API} *)

(** A base class for objects that can be queried at runtime for extra features. *)
module Generic : sig
  type 'a ty = ..
  (** An ['a ty] is a query for a feature of type ['a]. *)

  class type t = object
    method probe : 'a. 'a ty -> 'a option
  end

  val probe : #t -> 'a ty -> 'a option
end

(** Byte streams. *)
module Flow : sig
  type shutdown_command = [ `Receive | `Send | `All ]

  type read_method = ..
  (** Sources can offer a list of ways to read them, in order of preference. *)

  type read_method += Read_source_buffer of ((Cstruct.t list -> unit) -> unit)
  (** If a source offers [Read_source_buffer rsb] then the user can call [rsb fn]
      to borrow a view of the source's buffers.
      [rb] will raise [End_of_file] if no more data will be produced.
      If no data is currently available, [rb] will wait for some to become available before calling [fn].
      [fn] must not continue to use the buffers after it returns. *)

  class type close = object
    method close : unit
  end

  val close : #close -> unit

  (** Producer base class. *)
  class virtual source : object
    inherit Generic.t
    method virtual read_methods : read_method list
    method virtual read_into : Cstruct.t -> int
  end

  val read : #source -> Cstruct.t -> int
  (** [read src buf] reads one or more bytes into [buf].
      It returns the number of bytes written (which may be less than the
      buffer size even if there is more data to be read).
      [buf] must not be zero-length.

      [read src] just makes a single call to [src#read_into]
      (and asserts that the result is in range).
      Use {!read_exact} instead if you want to fill [buf] completely.
      Use {!Buf_read.line} to read complete lines.
      Use {!copy} to stream data directly from a source to a sink.
      @raise End_of_file if there is no more data to read *)

  val read_exact : #source -> Cstruct.t -> unit
  (** [read_exact src dst] keeps reading into [dst] until it is full.
      @raise End_of_file if the buffer could not be filled. *)

  val read_methods : #source -> read_method list
  (** [read_methods flow] is a list of extra ways of reading from [flow],
      with the preferred (most efficient) methods first.
      If no method is suitable, {!read} should be used as the fallback. *)

  val string_source : string -> source

  val cstruct_source : Cstruct.t list -> source

  (** Consumer base class. *)
  class virtual sink : object
    inherit Generic.t
    method virtual write : 'a. (#source as 'a) -> unit
  end

  val copy : #source -> #sink -> unit
  (** [copy src dst] copies data from [src] to [dst] until end-of-file. *)

  val copy_string : string -> #sink -> unit

  val buffer_sink : Buffer.t -> sink

  (** Bidirectional stream base class. *)
  class virtual two_way : object
    inherit source
    inherit sink

    method virtual shutdown : shutdown_command -> unit
  end

  val shutdown : #two_way -> shutdown_command -> unit
end

(** Buffered input and parsing *)
module Buf_read : sig
  (** This module provides fairly efficient non-backtracking parsers.
      It is modelled on Angstrom's API, and you should use that if
      backtracking is needed. *)

  type t

  exception Buffer_limit_exceeded

  type 'a parser = t -> 'a
  (** An ['a parser] is a function that consumes and returns a value of type ['a].
      @raise Failure The flow can't be parsed as a value of type ['a].
      @raise End_of_file The flow ended without enough data to parse an ['a].
      @raise Buffer_limit_exceeded The value was larger than the requested maximum buffer size. *)

  val parse : ?initial_size:int -> max_size:int -> 'a parser -> #Flow.source -> ('a, [> `Msg of string]) result
  (** [parse p flow ~max_size] uses [p] to parse everything in [flow].
      It is a convenience function that does
      [let buf = of_flow flow ~max_size in format_errors (p <* eof) buf]
      @param initial_size see {!of_flow}. *)

  val of_flow : ?initial_size:int -> max_size:int -> #Flow.source -> t
  (** [of_flow ~max_size flow] is a buffered reader backed by [flow].
      @param initial_size The initial amount of memory to allocate for the buffer.
      @param max_size The maximum size to which the buffer may grow.
                      This must be large enough to hold the largest single item
                      you want to parse (e.g. the longest line, if using
                      {!line}), plus any terminator needed to know the value is
                      complete (e.g. the newline character(s)). This is just to
                      prevent a run-away input from consuming all memory, and
                      you can usually just set it much larger than you expect
                      to need. *)

  val as_flow : t -> Flow.source
  (** [as_flow t] is a buffered flow. Reading from it will return data from the buffer,
      only reading the underlying flow if the buffer is empty. *)

  (** {2 Reading data} *)

  val line : string parser
  (** [line] parses one line.
      Lines can be terminated by either LF or CRLF.
      The returned string does not include the terminator.
      If [End_of_file] is reached after seeing some data but before seeing a line
      terminator, the data seen is returned as the last line. *)

  val lines : string Seq.t parser
  (** [lines] returns a sequence that lazily reads the next line until the end of the input is reached.
      [lines = seq line ~stop:at_end_of_input] *)

  val char : char -> unit parser
  (** [char c] checks that the next byte is [c] and consumes it.
      @raise Failure if the next byte is not [c] *)

  val any_char : char parser
  (** [any_char] parses one character. *)

  val peek_char : char option parser
  (** [peek_char] returns [Some c] where [c] is the next character, but does not consume it.
      Returns [None] at the end of the input stream rather than raising [End_of_file]. *)

  val string : string -> unit parser
  (** [string s] checks that [s] is the next string in the stream and consumes it.
      @raise Failure if [s] is not a prefix of the stream. *)

  val take : int -> string parser
  (** [take n] takes exactly [n] bytes from the input. *)

  val take_all : string parser
  (** [take_all] takes all remaining data until end-of-file.
      Returns [""] if already at end-of-file.
      @raise Buffer_limit_exceeded if the remaining data exceeds or equals the buffer limit
             (it needs one extra byte to confirm it has reached end-of-file). *)

  val take_while : (char -> bool) -> string parser
  (** [take_while p] finds the first byte for which [p] is false
      and consumes and returns all bytes before that.
      If [p] is true for all remaining bytes, it returns everything until end-of-file.
      It will return the empty string if there are no matching characters
      (and therefore never raises [End_of_file]). *)

  val skip_while : (char -> bool) -> unit parser
  (** [skip_while p] skips zero or more bytes for which [p] is [true].
      [skip_while p t] does the same thing as [ignore (take_while p t)],
      except that it is not limited by the buffer size. *)

  val skip : int -> unit parser
  (** [skip n] discards the next [n] bytes.
      [skip n] = [map ignore (take n)],
      except that the number of skipped bytes may be larger than the buffer (it will not grow).
      Note: if [End_of_file] is raised, all bytes in the stream will have been consumed. *)

  val at_end_of_input : bool parser
  (** [at_end_of_input] returns [true] when at the end of the stream, or
      [false] if there is at least one more byte to be read. *)

  val end_of_input : unit parser
  (** [end_of_input] checks that there are no further bytes in the stream.
      @raise Failure if there are further bytes *)

  (** {2 Combinators} *)

  val seq : ?stop:bool parser -> 'a parser -> 'a Seq.t parser
  (** [seq p] is a sequence that uses [p] to get the next item.
      @param stop This is used before parsing each item.
                  The sequence ends if this returns [true].
                  The default is {!at_end_of_input}. *)

  val pair : 'a parser -> 'b parser -> ('a * 'b) parser
  (** [pair a b] is a parser that first uses [a] to parse a value [x],
      then uses [b] to parse a value [y], then returns [(x, y)].
      Note that this module does not support backtracking, so if [b] fails
      then the bytes consumed by [a] are lost. *)

  val map : ('a -> 'b) -> ('a parser -> 'b parser)
  (** [map f a] is a parser that parses the stream with [a] to get [v],
      and then returns [f v]. *)

  val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
  (** [bind a f] is a parser that first uses [a] to parse a value [v],
      then uses [f v] to select the next parser, and then uses that. *)

  val format_errors : 'a parser -> ('a, [> `Msg of string]) result parser
  (** [format_errors p] catches [Failure], [End_of_file] and
      [Buffer_limit_exceeded] exceptions and returns them as a formatted error message. *)

  module Syntax : sig
    val ( let+ ) : 'a parser -> ('a -> 'b) -> 'b parser
    (** Syntax for {!map}. *)

    val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
    (** Syntax for {!bind} *)

    val ( and+ ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} *)

    val ( and* ) : 'a parser -> 'b parser -> ('a * 'b) parser
    (** Syntax for {!pair} (same as [and+]). *)

    val ( <* ) : 'a parser -> 'b parser -> 'a parser
    (** [a <* b] is [map fst (pair a b)].
        It parses two things and keeps only the first. *)

    val ( *> ) : 'a parser -> 'b parser -> 'b parser
    (** [a *> b] is [map snd (pair a b)].
        It parses two things and keeps only the second. *)
  end

  (** {2 Low-level API} *)

  val buffered_bytes : t -> int
  (** [buffered_bytes t] is the number of bytes that can be read without
      reading from the underlying flow. *)

  val peek : t -> Cstruct.t
  (** [peek t] returns a view onto the active part of [t]'s internal buffer.
      Performing any operation that might add to the buffer may invalidate this,
      so it should be used immediately and then forgotten.
      [Cstruct.length (peek t) = buffered_bytes t]. *)

  val ensure : t -> int -> unit
  (** [ensure t n] ensures that the buffer contains at least [n] bytes of data.
      If not, it reads from the flow until there is.
      [buffered_bytes (ensure t n) >= n].
      @raise End_of_file if the flow ended before [n] bytes were available
      @raise Buffer_limit_exceeded if [n] exceeds the buffer's maximum size *)

  val consume : t -> int -> unit
  (** [consume t n] discards the first [n] bytes from [t].
      [buffered_bytes t' = buffered_bytes t - n] *)

  val consumed_bytes : t -> int
  (** [consumed_bytes t] is the total number of bytes consumed.
      i.e. it is the offset into the stream of the next byte to be parsed. *)

  val eof_seen : t -> bool
  (** [eof_seen t] indicates whether we've received [End_of_file] from the underlying flow.
      If so, there will never be any further data beyond what [peek] already returns. *)
end

module Net : sig
  exception Connection_reset of exn

  module Ipaddr : sig
    type 'a t = private string
    (** The raw bytes of the IP address.
        It is either 4 bytes long (for an IPv4 address) or
        16 bytes long (for IPv6). *)

    module V4 : sig
      val any : [> `V4] t
      (** A special IPv4 address, for use only with [listen], representing
          all the Internet addresses that the host machine possesses. *)

      val loopback : [> `V4] t
      (** A special IPv4 address representing the host machine ([127.0.0.1]). *)
    end

    module V6 : sig
      val any : [> `V6] t
      (** A special IPv6 address, for use only with [listen], representing
          all the Internet addresses that the host machine possesses. *)

      val loopback : [> `V6] t
      (** A special IPv6 address representing the host machine ([::1]). *)
    end

    val pp : [< `V4 | `V6] t Fmt.t

    type v4v6 = [`V4 | `V6] t

    val classify :
      [< `V4 | `V6] t ->
      [ `V4 of [> `V4] t
      | `V6 of [> `V6] t]

    val of_raw : string -> v4v6
    (** [of_raw addr] casts [addr] to an IP address.
        @raise Invalid_argument if it is not 4 or 16 bytes long. *)
  end

  module Sockaddr : sig
    type stream = [
      | `Unix of string
      | `Tcp of Ipaddr.v4v6 * int
    ]
    (** Socket addresses that we can build a {! Flow.two_way} for i.e. stream-oriented
        protocols. *)

    type datagram = [
      | `Udp of Ipaddr.v4v6 * int
    ]
    (** Socket addresses that are message-oriented. *)

    type t = [ stream | datagram ]

    val pp : Format.formatter -> t -> unit
  end

  class virtual listening_socket : object
    inherit Generic.t
    method virtual close : unit
    method virtual accept : sw:Switch.t -> <Flow.two_way; Flow.close> * Sockaddr.stream
  end

  val accept :
    sw:Switch.t ->
    #listening_socket ->
    <Flow.two_way; Flow.close> * Sockaddr.stream
  (** [accept ~sw socket] waits until a new connection is ready on [socket] and returns it.
      The new socket will be closed automatically when [sw] finishes, if not closed earlier.
      If you want to handle multiple connections, consider using {!accept_sub} instead. *)

  val accept_sub :
    sw:Switch.t ->
    #listening_socket ->
    on_error:(exn -> unit) ->
    (sw:Switch.t -> <Flow.two_way; Flow.close> -> Sockaddr.stream -> unit) ->
    unit
  (** [accept_sub socket fn] waits for a new connection to [socket] and then runs [fn ~sw flow client_addr] in a new fibre,
      using {!Fibre.fork_on_accept}.
      [flow] will be closed automatically when the sub-switch is finished, if not already closed by then. *)

  class virtual endpoint : object
    method virtual send : Sockaddr.datagram -> Cstruct.t -> unit
    method virtual recv : Cstruct.t -> (Ipaddr.v4v6 * int) option * int
  end

  val send : #endpoint -> Sockaddr.datagram -> Cstruct.t -> unit
  (** [send e addr buf] sends the data in [buf] to the address [addr] using the endpoint [e]. *)

  val recv : #endpoint -> Cstruct.t -> (Ipaddr.v4v6 * int) option * int
  (** [recv e buf] receives data from the endpoint [e] putting it in [buf]. The number of bytes received is 
      returned along with the sender IP address and port if possible. If the [buf] is too small the read may 
      be partial. *)

  class virtual t : object
    method virtual listen : reuse_addr:bool -> reuse_port:bool -> backlog:int -> sw:Switch.t -> Sockaddr.stream -> listening_socket
    method virtual connect : sw:Switch.t -> Sockaddr.stream -> <Flow.two_way; Flow.close>
    method virtual endpoint : sw:Switch.t -> Sockaddr.datagram -> endpoint
  end

  val listen : ?reuse_addr:bool -> ?reuse_port:bool -> backlog:int -> sw:Switch.t -> #t -> Sockaddr.stream -> listening_socket
  (** [listen ~sw ~backlog t addr] is a new listening socket bound to local address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first.
      For (non-abstract) Unix domain sockets, the path will be removed afterwards.
      @param backlog The number of pending connections that can be queued up (see listen(2)).
      @param reuse_addr Set the [Unix.SO_REUSEADDR] socket option.
                        For Unix paths, also remove any stale left-over socket.
      @param reuse_port Set the [Unix.SO_REUSEPORT] socket option. *)

  val connect : sw:Switch.t -> #t -> Sockaddr.stream -> <Flow.two_way; Flow.close>
  (** [connect ~sw t addr] is a new socket connected to remote address [addr].
      The new socket will be closed when [sw] finishes, unless closed manually first. *)

  val endpoint : sw:Switch.t -> #t -> Sockaddr.datagram -> endpoint
  (** [endpoint ~sw t addr] creates a new, connectionless endpoint that data can be sent to
      and received from. The new socket will be closed when [sw] finishes. *)
end

module Domain_manager : sig
  class virtual t : object
    method virtual run_raw : 'a. (unit -> 'a) -> 'a

    method virtual run : 'a. (unit -> 'a) -> 'a
    (** Note: cancellation is handled by the {!run} wrapper function, not the object. *)
  end

  val run : #t -> (unit -> 'a) -> 'a
  (** [run t f] runs [f ()] in a newly-created domain and returns the result.
      Other fibres in the calling domain can run in parallel with the new domain.
      Warning: [f] must only access thread-safe values from the calling domain,
      but this is not enforced by the type system.
      If the calling fibre is cancelled, this is propagated to the spawned domain. *)

  val run_raw : #t -> (unit -> 'a) -> 'a
  (** [run_raw t f] is like {!run}, but does not run an event loop in the new domain,
      and so cannot perform IO, fork fibres, etc. *)
end

module Time : sig
  class virtual clock : object
    method virtual now : float
    method virtual sleep_until : float -> unit
  end

  val now : #clock -> float
  (** [now t] is the current time according to [t]. *)

  val sleep_until : #clock -> float -> unit
  (** [sleep_until t time] waits until the given time is reached. *)

  val sleep : #clock -> float -> unit
  (** [sleep t d] waits for [d] seconds. *)

  val with_timeout : #clock -> float -> (unit -> ('a, 'e) result) -> ('a, [> `Timeout] as 'e) result
  (** [with_timeout clock d fn] runs [fn ()] but cancels it after [d] seconds. *)

  exception Timeout

  val with_timeout_exn : #clock -> float -> (unit -> 'a) -> 'a
  (** [with_timeout_exn clock d fn] runs [fn ()] but cancels it after [d] seconds,
      raising exception [Timeout]. *)
end

module Unix_perm : sig
  type t = int
  (** This is the same as {!Unix.file_perm}, but avoids a dependency on [Unix]. *)
end

module Dir : sig
  type path = string

  exception Already_exists of path * exn
  exception Not_found of path * exn
  exception Permission_denied of path * exn

  class virtual rw : object
    inherit Generic.t
    inherit Flow.source
    inherit Flow.sink
  end

  type create = [`Never | `If_missing of Unix_perm.t | `Or_truncate of Unix_perm.t | `Exclusive of Unix_perm.t]
  (** When to create a new file:
      If [`Never] then it's an error if the named file doesn't exist.
      If [`If_missing] then an existing file is simply opened.
      If [`Or_truncate] then an existing file truncated to zero length.
      If [`Exclusive] then it is an error is the file does exist.
      If a new file is created, the given permissions are used for it. *)

  (** A [Dir.t] represents access to a directory and contents, recursively. *)
  class virtual t : object
    method virtual open_in : sw:Switch.t -> path -> <Flow.source; Flow.close>
    method virtual open_out :
      sw:Switch.t ->
      append:bool ->
      create:create ->
      path -> <rw; Flow.close>
    method virtual mkdir : perm:Unix_perm.t -> path -> unit
    method virtual open_dir : sw:Switch.t -> path -> t_with_close
  end
  and virtual t_with_close : object
    inherit t
    method virtual close : unit
  end

  val load : #t -> path -> string
  (** [load t path] returns the contents of the given file.
      This is a convenience wrapper around {!with_open_in}. *)

  val save : ?append:bool -> create:create -> #t -> path -> string -> unit
  (** [save t path data ~create] writes [data] to [path].
      This is a convenience wrapper around {!with_open_out}. *)

  val open_in : sw:Switch.t -> #t -> path -> <Flow.source; Flow.close>
  (** [open_in ~sw t path] opens [t/path] for reading.
      Note: files are always opened in binary mode. *)

  val with_open_in : #t -> path -> (<Flow.source; Flow.close> -> 'a) -> 'a
  (** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val with_lines : #t -> path -> (string Seq.t -> 'a) -> 'a
  (** [with_lines t path fn] is a convenience function for streaming the lines of the file. *)

  val open_out :
    sw:Switch.t ->
    ?append:bool ->
    create:create ->
    #t -> path -> <rw; Flow.close>
  (** [open_out ~sw t path] opens [t/path] for reading and writing.
      Note: files are always opened in binary mode.
      @param append Open for appending: always write at end of file.
      @param create Controls whether to create the file, and what permissions to give it if so. *)

  val with_open_out :
    ?append:bool ->
    create:create ->
    #t -> path -> (<rw; Flow.close> -> 'a) -> 'a
  (** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)

  val mkdir : #t -> perm:Unix.file_perm -> path -> unit
  (** [mkdir t ~perm path] creates a new directory [t/path] with permissions [perm]. *)

  val open_dir : sw:Switch.t -> #t -> path -> <t; Flow.close>
  (** [open_dir ~sw t path] opens [t/path].
      This can be passed to functions to grant access only to the subtree [t/path]. *)

  val with_open_dir : #t -> path -> (<t; Flow.close> -> 'a) -> 'a
  (** [with_open_dir] is like [open_dir], but calls [fn dir] with the new directory and closes
      it automatically when [fn] returns (if it hasn't already been closed by then). *)
end

(** The standard environment of a process. *)
module Stdenv : sig
  type t = <
    stdin  : Flow.source;
    stdout : Flow.sink;
    stderr : Flow.sink;
    net : Net.t;
    domain_mgr : Domain_manager.t;
    clock : Time.clock;
    fs : Dir.t;
    cwd : Dir.t;
    secure_random : Flow.source;
  >

  val stdin  : <stdin  : #Flow.source as 'a; ..> -> 'a
  val stdout : <stdout : #Flow.sink   as 'a; ..> -> 'a
  val stderr : <stderr : #Flow.sink   as 'a; ..> -> 'a

  val net : <net : #Net.t as 'a; ..> -> 'a
  val domain_mgr : <domain_mgr : #Domain_manager.t as 'a; ..> -> 'a
  val clock : <clock : #Time.clock as 'a; ..> -> 'a

  val secure_random : <secure_random : #Flow.source as 'a; ..> -> 'a
  (** [secure_random t] is a source of random bytes suitable for cryptographic purposes. *)

  val cwd : <cwd : #Dir.t as 'a; ..> -> 'a
  (** [cwd t] is the current working directory of the process (this may change
      over time if the process does a `chdir` operation, which is not recommended). *)

  val fs : <fs : #Dir.t as 'a; ..> -> 'a
  (** [fs t] is the process's full access to the filesystem.
      Paths can be absolute or relative (to the current working directory).
      Using relative paths with this is similar to using them with {!cwd},
      except that this will follow symlinks to other parts of the filesystem.
      [fs] is useful for handling paths passed in by the user. *)
end

(** {1 Provider API for OS schedulers} *)

(** API for use by the scheduler implementation. *)
module Private : sig
  (** Every fibre has an associated context. *)
  module Fibre_context : sig
    type t

    val make_root : unit -> t
    (** Make a new root context for a new domain. *)

    val make : cc:Cancel.t -> t
    (** [make ~cc] is a new fibre context, initially attached to the given cancellation context. *)

    val destroy : t -> unit
    (** [destroy t] removes [t] from its cancellation context. *)

    val tid : t -> Ctf.id

    val cancellation_context : t -> Cancel.t
    (** [cancellation_context t] is [t]'s current cancellation context. *)

    val set_cancel_fn : t -> (exn -> unit) -> unit
    (** [set_cancel_fn t fn] sets [fn] as the fibre's cancel function.
        If the cancellation context is cancelled, the function is removed and called.
        When the operation completes, you must call {!clear_cancel_fn} to remove it. *)

    val clear_cancel_fn : t -> bool
    (** [clear_cancel_fn t] removes the function previously set with {!set_cancel_fn}, if any.
        Returns [true] if this call removed the function, or [false] if there wasn't one.
        This operation is atomic and thread-safe.
        An operation that completes in another domain must use this to indicate that the operation is
        finished (can no longer be cancelled) before enqueuing the result. If it returns [false],
        the operation was cancelled first and the canceller has called (or is calling) the function.
        If it returns [true], the caller is responsible for any resources owned by the function,
        such as the continuation. *)

    val get_error : t -> exn option
    (** [get_error t] is [Cancel.get_error (cancellation_context t)] *)
  end

  module Effects : sig
    open Effect

    type 'a enqueue = ('a, exn) result -> unit
    (** A function provided by the scheduler to reschedule a previously-suspended thread. *)

    type _ eff +=
      | Suspend : (Fibre_context.t -> 'a enqueue -> unit) -> 'a eff
      (** [Suspend fn] is performed when a fibre must be suspended
          (e.g. because it called {!Promise.await} on an unresolved promise).
          The effect handler runs [fn fibre enqueue] in the scheduler context,
          passing it the suspended fibre's context and a function to resume it.
          [fn] should arrange for [enqueue] to be called once the thread is ready to run again. *)

      | Fork : Fibre_context.t * (unit -> unit) -> unit eff
      (** See {!Fibre.fork} *)

      | Trace : (?__POS__:(string * int * int * int) -> ('a, Format.formatter, unit, unit) format4 -> 'a) eff
      (** [perform Trace fmt] writes trace logging to the configured trace output.
          It must not switch fibres, as tracing must not affect scheduling.
          If the system is not ready to receive the trace output,
          the whole domain must block until it is. *)

      | Get_context : Fibre_context.t eff
  end

  (** Temporary hack for compatibility with ocaml.4.12+domains *)
  module Effect = Effect
end
