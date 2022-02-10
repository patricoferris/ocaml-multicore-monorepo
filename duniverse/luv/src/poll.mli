(* This file is part of Luv, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/aantron/luv/blob/master/LICENSE.md. *)



(** File descriptor polling.

    See {{:http://docs.libuv.org/en/v1.x/poll.html} [uv_poll_t] {i — Poll
    handle}} in libuv. *)

type t = [ `Poll ] Handle.t
(** Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_t}
    [uv_poll_t]}.

    Note that values of this type can be passed to functions in {!Luv.Handle},
    in addition to the functions in this module. In particular, see
    {!Luv.Handle.close}. *)

val init : ?loop:Loop.t -> int -> (t, Error.t) result
(** Allocates and initializes a polling handle.

    Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_init}
    [uv_poll_init]}. *)

val init_socket : ?loop:Loop.t -> Os_fd.Socket.t -> (t, Error.t) result
(** Creates a polling handle for an existing socket.

    Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_init_socket}
    [uv_poll_init_socket]}. *)

(** Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_event}
    [uv_poll_event]}. *)
module Event :
sig
  type t = [
    | `READABLE
    | `WRITABLE
    | `DISCONNECT
    | `PRIORITIZED
  ]
  (** Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_event}
      [uv_poll_event]}.

      [`DISCONNECT] is implemented starting with libuv 1.9.0. On earlier
      versions, trying to register for [`DISCONNECT] events does nothing, and,
      correspondingly, user code cannot receive a [`DISCONNECT] event. The OCaml
      code will still compile.

      Similarly, [`PRIORITIZED] requires libuv 1.14.0.

      {{!Luv.Require} Feature checks}:

      - [Luv.Require.(has disconnect)]
      - [Luv.Require.(has prioritized)] *)
end

val start :
  t -> Event.t list -> ((Event.t list, Error.t) result -> unit) -> unit
(** Starts polling the underlying descriptor.

    Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_start}
    [uv_poll_start]}. *)

val stop : t -> (unit, Error.t) result
(** Stops polling.

    Binds {{:http://docs.libuv.org/en/v1.x/poll.html#c.uv_poll_stop}
    [uv_poll_stop]}. *)
