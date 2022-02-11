(* Copyright (C) 2018--2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

module Sys = struct
  type 'a future = 'a
  let return x = x
  let or_fail = Caqti_blocking.or_fail
  module Infix = struct
    let (>>=) x f = f x
    let (>|=) x f = f x
  end
end

module Test = Test_sql.Make (Sys) (Caqti_blocking)

let test_on uri =
  try
    Caqti_blocking.connect uri |> Sys.or_fail |> Test.run;
    (match Caqti_blocking.connect_pool ~post_connect:Test.post_connect uri with
     | Error err -> raise (Caqti_error.Exn err)
     | Ok pool -> Test.run_pool pool)
  with
   | Caqti_error.Exn err ->
      eprintf "%s\n" (Caqti_error.show err);
      exit 2
   | exn ->
      eprintf "%s raised during test on %s\n"
        (Printexc.to_string exn) (Uri.to_string uri);
      exit 2

let () = List.iter test_on (Testkit.parse_common_args ())
