open Effect

class virtual t = object
  method virtual run : 'a. (unit -> 'a) -> 'a
  method virtual run_raw : 'a. (unit -> 'a) -> 'a
end

let run_raw (t : #t) = t#run_raw

let run (t : #t) fn =
  let ctx = perform Cancel.Get_context in
  Cancel.check ctx.cancel_context;
  let cancelled, set_cancelled = Promise.create () in
  Cancel.Fiber_context.set_cancel_fn ctx (Promise.resolve set_cancelled);
  (* If the spawning fiber is cancelled, [cancelled] gets set to the exception. *)
  match
    t#run @@ fun () ->
    Fiber.first
      (fun () ->
         match Promise.await cancelled with
         | Cancel.Cancelled ex -> raise ex    (* To avoid [Cancelled (Cancelled ex))] *)
         | ex -> raise ex (* Shouldn't happen *)
      )
      fn
  with
  | x ->
    ignore (Cancel.Fiber_context.clear_cancel_fn ctx : bool);
    x
  | exception ex ->
    ignore (Cancel.Fiber_context.clear_cancel_fn ctx : bool);
    match Promise.peek cancelled with
    | Some (Cancel.Cancelled ex2 as cex) when ex == ex2 ->
      (* We unwrapped the exception above to avoid a double cancelled exception.
         But this means that the top-level reported the original exception,
         which isn't what we want. *)
      raise cex
    | _ -> raise ex
