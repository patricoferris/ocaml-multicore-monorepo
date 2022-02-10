(* This file is part of Luv, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/aantron/luv/blob/master/LICENSE.md. *)



(** @since Luv 0.5.3 *)
module Request :
sig
  type t = [ `Random ] Request.t
  val make : unit -> t
end

val random :
  ?loop:Loop.t ->
  ?request:Request.t ->
  Buffer.t ->
  ((unit, Error.t) result -> unit) ->
    unit
(** Fills the given buffer with bits from the system entropy source.

    Binds {{:http://docs.libuv.org/en/v1.x/misc.html#c.uv_random}
    [uv_random]}.

    Requires libuv 1.33.0.

    {{!Luv.Require} Feature check}: [Luv.Require.(has random)] *)

module Sync :
sig
  val random : Buffer.t -> (unit, Error.t) result
  (** Synchronous version of {!Luv.Random.random}. *)
end
