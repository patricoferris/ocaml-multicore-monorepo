# Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run (fn : clock:Eio.Time.clock -> unit) =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  fn ~clock
```

# Test cases

Check sleep works:

```ocaml
# run @@ fun ~clock ->
  let t0 = Unix.gettimeofday () in
  Eio.Time.sleep clock 0.01;
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.01);;
- : unit = ()
```

Check sleep works with a switch:

```ocaml
# run @@ fun ~clock ->
  let t0 = Unix.gettimeofday () in
  Eio.Time.sleep clock 0.01;
  let t1 = Unix.gettimeofday () in
  assert (t1 -. t0 >= 0.01);;
- : unit = ()
```

Cancelling sleep:

```ocaml
# run @@ fun ~clock ->
  Fibre.both
    (fun () -> Eio.Time.sleep clock 1200.; assert false)
    (fun () -> failwith "Simulated cancel");;
Exception: Failure "Simulated cancel".
```

Switch is already off:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Switch.fail sw (Failure "Simulated failure");
  Eio.Time.sleep clock 1200.0;
  assert false;;
Exception: Failure "Simulated failure".
```

Scheduling a timer that's already due:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Fibre.both
    (fun () -> traceln "First fibre runs"; Eio.Time.sleep clock (-1.0); traceln "Sleep done")
    (fun () -> traceln "Second fibre runs");;
+First fibre runs
+Second fibre runs
+Sleep done
- : unit = ()
```

Check ordering works:

```ocaml
# run @@ fun ~clock ->
  Switch.run @@ fun sw ->
  Fibre.both
    (fun () ->
      Eio.Time.sleep clock 1200.0;
      assert false
    )
    (fun () ->
      Eio.Time.sleep clock 0.1;
      traceln "Short timer finished";
      failwith "Simulated cancel"
    );;
+Short timer finished
Exception: Failure "Simulated cancel".
```
