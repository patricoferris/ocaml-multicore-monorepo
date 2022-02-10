(* This file is part of Luv, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/aantron/luv/blob/master/LICENSE.md. *)



let pp_error_code formatter error_code =
  Format.fprintf
    formatter
    "%s (%s)"
    (Luv.Error.strerror error_code)
    (Luv.Error.err_name error_code)

let error_code_testable =
  Alcotest.of_pp pp_error_code

let fail_with_error_code name error_code =
  Fmt.to_to_string pp_error_code error_code
  |> Alcotest.failf "%s failed with %s" name

let check_error_code name expected error_code =
  Alcotest.check error_code_testable name expected error_code

let check_success_result name result =
  match result with
  | Result.Ok value -> value
  | Result.Error code -> fail_with_error_code name code

let check_error_result name expected result =
  Alcotest.(check (result reject error_code_testable))
    name (Result.Error expected) result

let check_error_results name expected result =
  match result with
  | Result.Ok _ ->
    Alcotest.failf "%s: expected Error _; got Ok _." name
  | Result.Error code ->
    if not @@ List.mem code expected then
      Alcotest.failf "%s: expected %s; got %s."
        name
        (List.map Luv.Error.err_name expected |> String.concat ", ")
        (Luv.Error.err_name code)

let pointer_testable =
  let format formatter pointer =
    if Ctypes.is_null pointer then
      Format.pp_print_string formatter "null"
    else
      Format.fprintf formatter "%nX" (Ctypes.raw_address_of_ptr pointer)
  in

  let equal pointer pointer' =
    Ctypes.raw_address_of_ptr pointer = Ctypes.raw_address_of_ptr pointer'
  in

  Alcotest.testable format equal

let check_not_null name pointer =
  Alcotest.check
    (Alcotest.neg pointer_testable) name Ctypes.null (Ctypes.to_voidp pointer)

let check_pointer name expected actual =
  Alcotest.check
    pointer_testable name (Ctypes.to_voidp expected) (Ctypes.to_voidp actual)

let pp_directory_entry formatter entry =
  let kind =
    let open Luv.File.Dirent in
    match entry.kind with
    | `FILE -> "file"
    | `DIR -> "dir"
    | `LINK -> "link"
    | `FIFO -> "fifo"
    | `SOCKET -> "socket"
    | `CHAR -> "char"
    | `BLOCK -> "block"
    | `UNKNOWN -> "unknown"
  in
  Format.fprintf formatter "%s %s" kind Luv.File.Dirent.(entry.name)

let directory_entry_testable =
  Alcotest.of_pp pp_directory_entry

let check_directory_entries name expected actual =
  let expected =
    expected
    |> List.map (fun name -> Luv.File.Dirent.{kind = `FILE; name})
    |> List.sort compare
  in
  let actual = List.sort compare actual in
  Alcotest.(check (list directory_entry_testable)) name expected actual

let count_allocated_words () =
  Gc.full_major ();
  Gc.((stat ()).live_words)

let count_allocated_words_during repetitions f =
  let initial = count_allocated_words () in
  for i = 1 to repetitions do
    f i
  done;
  max 0 (count_allocated_words () - initial)

let callback_index = ref 0
let accumulator = ref 0

let make_callback () =
  let index = !callback_index in
  callback_index := !callback_index + 1;

  fun _ ->
    accumulator := !accumulator + index

let no_memory_leak ?(base_repetitions = 100) f =
  let allocated_during_first_run =
    count_allocated_words_during base_repetitions f
    |> float_of_int
  in
  let allocated_during_second_run =
    count_allocated_words_during (base_repetitions * 3) f
    |> float_of_int
  in

  if allocated_during_second_run /. allocated_during_first_run > 1.1 then
    Alcotest.failf
      "memory leak: %.0f, then %.0f words allocated"
      allocated_during_first_run
      allocated_during_second_run

let default_loop =
  Luv.Loop.default ()

let run ?(with_timeout = false) () =
  if with_timeout then begin
    let timeout =
      Luv.Timer.init ()
      |> check_success_result "run timeout init"
    in

    let stop = ref false in

    Luv.Loop.update_time default_loop;
    Luv.Timer.start timeout 5000 (fun _ -> stop := true)
    |> check_success_result "timeout timer start";

    let rec run () =
      if !stop then ()
      else
        if Luv.Loop.run ~mode:`ONCE () then run ()
        else ()
    in
    run ()
  end
  else
    Luv.Loop.run ()
    |> ignore

let port = ref 5000
let port () =
  port := !port + 1;
  !port

let fresh_address () =
  Luv.Sockaddr.ipv4 "127.0.0.1" (port ()) |> check_success_result "ipv4"

let pp_exception formatter exn =
  Format.pp_print_string formatter (Printexc.to_string exn)

let exception_testable =
  Alcotest.of_pp pp_exception

let () =
  Luv.Error.set_on_unhandled_exception raise

exception Nothing_raised

let check_exception expected f =
  let raised = ref Nothing_raised in

  Luv.Error.set_on_unhandled_exception ((:=) raised);
  f ();
  Luv.Error.set_on_unhandled_exception raise;

  Alcotest.(check exception_testable) "exception" expected !raised

type event = [
  | `Deferred of unit -> unit
  | `Proceed
] ref

let event () =
  ref (`Deferred ignore)

let defer p f =
  match !p with
  | `Deferred _ -> p := `Deferred f
  | `Proceed -> f ()

let proceed p =
  match !p with
  | `Deferred f -> f ()
  | `Proceed -> ()

let in_travis =
  match Sys.getenv "TRAVIS" with
  | "true" -> true
  | _ -> false
  | exception Not_found -> false
