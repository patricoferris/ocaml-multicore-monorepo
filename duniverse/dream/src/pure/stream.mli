(* This file is part of Dream, released under the MIT license. See LICENSE.md
   for details, or visit https://github.com/aantron/dream.

   Copyright 2021 Anton Bachin *)



(* Note: this is not a stable API! *)



type reader
type writer
type stream

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type 'a promise =
  'a Lwt.t

type read =
  data:(buffer -> int -> int -> bool -> bool -> unit) ->
  close:(int -> unit) ->
  flush:(unit -> unit) ->
  ping:(buffer -> int -> int -> unit) ->
  pong:(buffer -> int -> int -> unit) ->
    unit
(** A reading function. Awaits the next event on the stream. For each call of a
    reading function, one of the callbacks will eventually be called, according
    to which event occurs next on the stream. *)

type write =
  close:(int -> unit) ->
  (unit -> unit) ->
    unit
(** A writing function. Pushes an event into a stream. May take additional
    arguments before [~ok]. *)

val reader : read:read -> close:(int -> unit) -> reader
(** Creates a read-only stream from the given reader. [~close] is called in
    response to {!Stream.close}. It doesn't need to call {!Stream.close} again
    on the stream. It should be used to free any underlying resources. *)

val pipe : unit -> reader * writer
(** A stream which matches each call of the reading function to one call of its
    writing functions. For example, calling {!Stream.flush} on a pipe will cause
    the reader to call its [~flush] callback. *)

val forward : reader -> stream -> unit

val no_reader : reader

val no_writer : writer

val stream : reader -> writer -> stream
(* TODO Consider tupling the arguments, as that will make it easier to pass the
   result of Stream.pipe. *)

val null : stream
(** A stream which is neither readable nor writable. *)

val empty : stream
(** A read-only stream whose reading function always calls its [~close]
    callback. *)

val string : string -> stream
(** A read-only stream which calls its [~data] callback once with the contents
    of the given string, and then always calls [~close]. *)

val close : stream -> int -> unit
(** Closes the given stream. Causes a pending reader or writer to call its
    [~close] callback. *)

val read : stream -> read
(** Awaits the next stream event. See {!Stream.type-read}. *)

val read_convenience : stream -> string option promise
(** A wrapper around {!Stream.read} that converts [~data] with content [s] into
    [Some s], and [~close] into [None], and uses them to resolve a promise.
    [~flush] is ignored. *)

val read_until_close : stream -> string promise
(** Reads a stream completely until [~close], and accumulates the data into a
    string. *)

val ready : stream -> write

val write : stream -> buffer -> int -> int -> bool -> bool -> write
(** A writing function that sends a data buffer on the given stream. No more
    writing functions should be called on the stream until this function calls
    [~ok]. The [bool] arguments are whether the message is binary and whether
    the [FIN] flag should be set. They are ignored by non-WebSocket streams.

    Note: [FIN] is provided as part of the write call, rather than being a
    separate stream event (like [flush]), because the WebSocket writer needs to
    immediately know when the last chunk of the last frame in a message is
    provided, to transmit the [FIN] bit. If [FIN] were to be provided as a
    separate event, the WebSocket writer would have to buffer each one chunk, in
    case the next stream event was [FIN], in order to be able to decide whether
    to set the [FIN] bit or not. This is awkward and inefficient, as it
    introduces an unnecessary delay into the writer, as if the next event is not
    [FIN], the next data chunk might take an arbitrary amount of time to be
    generated by the writing user code. *)

val flush : stream -> write
(** A writing function that asks for the given stream to be flushed. The meaning
    of flushing depends on the implementation of the stream. No more writing
    functions should be called on the stream until this function calls [~ok]. *)

val ping : stream -> buffer -> int -> int -> write
(** A writing function that sends a ping event on the given stream. This is only
    meaningful for WebSockets. *)

val pong : stream -> buffer -> int -> int -> write
(** A writing function that sends a pong event on the given stream. This is only
    meaningful for WebSockets. *)
