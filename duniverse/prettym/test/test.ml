external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "90EMsUCFv4vRTbJGY+Y3ijLDsMBPDx7DH85k5CwdYfE="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
  Random.full_init random_seed

let comma = ((fun t () -> Prettym.char t ','), ())

let rec value t x =
  let binding t (k, v) =
    Prettym.eval t
      Prettym.[ char $ '"'; !!string; char $ '"'; char $ ':'; !!value ]
      k v
  in
  let arr = Prettym.list ~sep:comma value in
  let obj = Prettym.list ~sep:comma binding in
  match x with
  | `Bool true -> Prettym.string t "true"
  | `Bool false -> Prettym.string t "false"
  | `Null -> Prettym.string t "null"
  | `Float f -> Prettym.string t (Fmt.str "%.16g" f)
  | `String s -> Prettym.eval t Prettym.[ char $ '"'; !!string; char $ '"' ] s
  | `A a -> Prettym.eval t Prettym.[ char $ '['; !!arr; char $ ']' ] a
  | `O o -> Prettym.eval t Prettym.[ char $ '{'; !!obj; char $ '}' ] o

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]
type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let json_of_input refiller input =
  let decoder = Jsonm.decoder input in
  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in
  let rec arr acc k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        arr acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        name n k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme v -> base (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        obj acc k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> Fmt.invalid_arg "Unexpected lexeme: %a" Jsonm.pp_lexeme v
  and base k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> Fmt.invalid_arg "Unexpected end of array/object"
    | `Name v -> Fmt.invalid_arg "Unexpected key: %s" v
  in
  let rec go k =
    match Jsonm.decode decoder with
    | #await ->
        refiller ();
        go k
    | #error as err -> error err
    | #eoi as eoi -> end_of_input eoi
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme
  in
  go (fun x -> x)

let json_to_output flusher output json =
  let encoder = Jsonm.encoder output in
  let flat_json json : Jsonm.lexeme list =
    let rec arr acc k = function
      | [] -> k (List.rev (`Ae :: acc))
      | (#value as x) :: r -> arr (x :: acc) k r
      | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
      | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l
    and obj acc k = function
      | [] -> k (List.rev (`Oe :: acc))
      | (n, x) :: r ->
          base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x
    and base k = function
      | `A l -> arr [ `As ] k l
      | `O l -> obj [ `Os ] k l
      | #value as x -> k [ x ]
    in
    base (fun l -> l) json
  in
  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
        flusher (Jsonm.Manual.dst_rem encoder);
        write k (Jsonm.encode encoder `Await)
  in
  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r ->
        write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme))
  in
  let lexemes = flat_json json in
  go (fun x -> x) lexemes

let json_of_string x =
  (* XXX(dinosaure): [Jsonm] does not reach [`Await]/[refiller] case if input is
     [`String]. *)
  json_of_input (fun () -> assert false) (`String x)

let json_to_string x =
  let buf = Buffer.create 0x100 in
  (* XXX(dinosaure): [Jsonm] does not reach [`Partial]/[flusher] case if output
     os [`Buffer]. *)
  json_to_output (fun _ -> assert false) (`Buffer buf) x;
  Buffer.contents buf

let tests =
  [
    `A [ `Bool true ]; `O [ ("a", `A [ `Bool true; `Bool false ]) ];
    `A [ `O [ ("a", `Bool true); ("b", `Bool false) ] ];
  ]

let json = Alcotest.testable (Fmt.using json_to_string Fmt.string) ( = )

let make v =
  Alcotest.test_case (json_to_string v) `Quick @@ fun () ->
  let buf = Buffer.create 0x100 in

  let emitter =
    let write a x =
      let open Prettym.IOVec in
      let open Prettym.Buffer in
      match x with
      | { buffer = String x; off; len } ->
          Buffer.add_substring buf x off len;
          a + len
      | { buffer = Bytes x; off; len } ->
          Buffer.add_subbytes buf x off len;
          a + len
      | { buffer = Bigstring x; off; len } ->
          let x = Bigstringaf.substring x ~off ~len in
          Buffer.add_string buf x;
          a + len
    in
    List.fold_left write 0
  in

  let encoder = Prettym.create ~emitter ~margin:78 ~new_line:"\n" 0x100 in

  let kend encoder =
    if Prettym.is_empty encoder then ()
    else Fmt.failwith "Leave a non-empty encoder"
  in

  let () = Prettym.keval kend encoder Prettym.[ !!value; new_line ] v in
  let res = Buffer.contents buf in
  let res = json_of_string res in
  Alcotest.(check json) "encode:decode:compare" v res

let unroll_box () =
  Alcotest.test_case "unroll" `Quick @@ fun () ->
  let g ppf (k, x, y) =
    let open Prettym in
    eval ppf
      [
        !!string; char $ ':'; tbox 1; !!string; cut; char $ '&'; cut; !!string;
        close; new_line;
      ]
      k x y
  in
  let result = Prettym.to_string ~margin:14 g ("AAAAAA", "BBBBBB", "CCCCCC") in
  Alcotest.(check string) "result" result "AAAAAA:\r\n BBBBBB&CCCCCC\r\n"
(* XXX(dinosaure): instead to cut at [&], it unroll the [tbox] and print elements to the new line. *)

let random_word max =
  let len = 1 + Random.int max in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    match Random.int (26 + 26 + 10) with
    | n when n < 26 -> Bytes.set res i (Char.chr (Char.code 'a' + n))
    | n when n < 26 + 26 -> Bytes.set res i (Char.chr (Char.code 'A' + n - 26))
    | n -> Bytes.set res i (Char.chr (Char.code '0' + n - 26 - 26))
  done;
  Bytes.unsafe_to_string res

let lift () =
  Alcotest.test_case "lift" `Quick @@ fun () ->
  let sep ppf () =
    let open Prettym in
    eval ppf [ spaces 1 ]
  in
  let pp ppf lst =
    let open Prettym in
    eval ppf [ !!(list ~sep:(sep, ()) string) ] lst
  in
  let lst = List.init 500 (fun _ -> random_word 14) in
  let res = Prettym.to_string ~margin:78 pp lst in
  let check line =
    Alcotest.(check bool) "< 80" (String.length line < 80) true
  in
  List.iter check (Astring.String.cuts ~sep:"\r\n" res)

let () =
  Alcotest.run "format"
    [
      ("json", List.map make tests); ("unroll", [ unroll_box () ]);
      ("lift", [ lift () ]);
    ]
