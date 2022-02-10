(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let output ~per_file ~coverage_files ~coverage_paths ~expect ~do_not_expect =
  let coverage =
    Input.load_coverage
      ~coverage_files ~coverage_paths ~expect ~do_not_expect in

  let stats =
    Hashtbl.fold (fun _ (file : Bisect_common.instrumented_file) acc ->
      let total = Array.length file.counts in
      let visited =
        Array.fold_left
          (fun acc count -> if count > 0 then acc + 1 else acc) 0 file.counts
      in
      (file.filename, visited, total)::acc) coverage []
  in

  let percentage numerator denominator =
    if denominator > 0 then
      let p =
        ((float_of_int numerator) *. 100.) /. (float_of_int denominator) in
      Printf.sprintf "%.2f" p
    else
      "100.00"
  in

  let second (_, v, _) = v in
  let third (_, _, v) = v in

  let total projection =
    stats
    |> List.map projection
    |> List.fold_left (+) 0
  in
  let visited_total = total second in
  let overall_total = total third in

  if per_file then begin
    let digits i =
      let rec loop bound count =
        if bound > i then
          count
        else
          loop (bound * 10) (count + 1)
      in
      loop 10 1
    in
    let digits projection =
      ("", visited_total, overall_total)::stats
      |> List.map projection
      |> List.map digits
      |> List.fold_left max 1
    in
    let visited_digits = digits second in
    let total_digits = digits third in

    stats
    |> List.sort (fun (file, _, _) (file', _, _) -> String.compare file file')
    |> List.iter begin fun (name, visited, total) ->
      Printf.printf "%6s %%   %*i/%-*i   %s\n"
        (percentage visited total)
        visited_digits visited
        total_digits total
        name
    end;

    Printf.printf "%6s %%   %i/%i   Project coverage\n%!"
      (percentage visited_total overall_total) visited_total overall_total
  end
  else
    Printf.printf "Coverage: %i/%i (%s%%)\n%!"
      visited_total overall_total (percentage visited_total overall_total)
