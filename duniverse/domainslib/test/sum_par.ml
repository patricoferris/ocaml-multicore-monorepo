let num_domains = try int_of_string Sys.argv.(1) with _ -> 2
let n = try int_of_string Sys.argv.(2) with _ -> 100

module T = Domainslib.Task

let _ =
  (* use parallel_for_reduce *)
  let p = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
  let sum = T.run p (fun _ ->
    T.parallel_for_reduce p (+) 0 ~chunk_size:(n/(4*num_domains)) ~start:0
      ~finish:(n-1) ~body:(fun _i -> 1))
  in
  T.teardown_pool p;
  Printf.printf "Sum is %d\n" sum;
  assert (sum = n)

let _ =
  (* explictly use empty pool and default chunk_size *)
  let p = T.setup_pool ~num_additional_domains:0 () in
  let sum = Atomic.make 0 in
  T.run p (fun _ ->
    T.parallel_for p ~start:0 ~finish:(n-1)
        ~body:(fun _i -> ignore (Atomic.fetch_and_add sum 1)));
  let sum = Atomic.get sum in
  T.teardown_pool p;
  Printf.printf "Sum is %d\n" sum;
  assert (sum = n)

let _ =
  (* configured num_domains and default chunk_size *)
  let p = T.setup_pool ~num_additional_domains:(num_domains - 1) () in
  let sum = Atomic.make 0 in
  T.run p (fun _ ->
    T.parallel_for p ~start:0 ~finish:(n-1)
        ~body:(fun _i -> ignore (Atomic.fetch_and_add sum 1)));
  let sum = Atomic.get sum in
  T.teardown_pool p;
  Printf.printf "Sum is %d\n" sum;
  assert (sum = n)

