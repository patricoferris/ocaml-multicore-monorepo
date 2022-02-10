(* This file is part of Luv, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/aantron/luv/blob/master/LICENSE.md. *)



type t = {
  username : string;
  uid : int;
  gid : int;
  shell : string option;
  homedir : string;
}
(** Binds {{:http://docs.libuv.org/en/v1.x/misc.html#c.uv_passwd_t}
    [uv_passwd_t]}. *)

val get_passwd : unit -> (t, Error.t) result
(** Gets passwd entry for the current user.

    Binds {{:http://docs.libuv.org/en/v1.x/misc.html#c.uv_os_get_passwd}
    [uv_os_get_passwd]}. See
    {{:http://man7.org/linux/man-pages/man3/getpwuid_r.3p.html}
    [getpwuid_r(3p)]}.

    Requires libuv 1.9.0.

    {{!Luv.Require} Feature check}: [Luv.Require.(has os_get_passwd)] *)
