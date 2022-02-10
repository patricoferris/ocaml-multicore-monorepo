(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml
open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len
    then []
    else if Char.equal p.[cur] '\000'
    then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
    else split beg (cur + 1)
  in
  Array.of_list (split 0 0)

let new_directive name k = Hashtbl.add Toploop.directive_table name k
  [@@alert "-deprecated"]

let setup =
  lazy
     (Topdirs.dir_directory "/static/cmis";
     new_directive "enable" (Toploop.Directive_string Config.Flag.enable);
     new_directive "disable" (Toploop.Directive_string Config.Flag.disable);
     new_directive "debug_on" (Toploop.Directive_string Debug.enable);
     new_directive "debug_off" (Toploop.Directive_string Debug.disable);
     new_directive "tailcall" (Toploop.Directive_string (Config.Param.set "tc"));
     let initial_primitive_count =
       Array.length (split_primitives (Symtable.data_primitive_names ()))
     in
     (* this needs to stay synchronized with toplevel.js *)
     let compile (s : bytes array) =
       let s = String.concat ~sep:"" (List.map ~f:Bytes.to_string (Array.to_list s)) in
       let prims = split_primitives (Symtable.data_primitive_names ()) in
       let unbound_primitive p =
         try
           ignore (Js.Unsafe.eval_string p);
           false
         with _ -> true
       in
       let stubs = ref [] in
       Array.iteri prims ~f:(fun i p ->
           if i >= initial_primitive_count && unbound_primitive p
           then
             stubs :=
               Format.sprintf "function %s(){caml_failwith(\"%s not implemented\")}" p p
               :: !stubs);
       let output_program = Driver.from_string prims s in
       let b = Buffer.create 100 in
       output_program (Pretty_print.to_buffer b);
       Format.(pp_print_flush std_formatter ());
       Format.(pp_print_flush err_formatter ());
       flush stdout;
       flush stderr;
       let res = Buffer.contents b in
       let res = String.concat ~sep:"" !stubs ^ res in
       let res : unit -> _ = Js.Unsafe.global##toplevelEval (res : string) in
       res
     in
     Js.Unsafe.global##.toplevelCompile := compile (*XXX HACK!*);
     (Js.Unsafe.global##.toplevelEval
     := fun (x : string) ->
     let f : < .. > Js.t -> < .. > Js.t = Js.Unsafe.eval_string x in
     fun () ->
       let res = f Js.Unsafe.global in
       Format.(pp_print_flush std_formatter ());
       Format.(pp_print_flush err_formatter ());
       flush stdout;
       flush stderr;
       res);
     Js.Unsafe.global##.toplevelReloc
     := Js.Unsafe.callback (fun name ->
            let name = Js.to_string name in
            Js_of_ocaml_compiler.Ocaml_compiler.Symtable.reloc_ident name);
     ()) 

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    String.blit ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        if nl then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

let toploop_use_silently ffp name = Toploop.use_silently ffp name
  [@@ocaml.warning "-32"] [@@if ocaml_version < (4, 14, 0)]

let toploop_use_silently ffp name = Toploop.use_silently ffp (File name)
  [@@ocaml.warning "-32"] [@@if ocaml_version >= (4, 14, 0)]

let use ffp content =
  let name = "/dev/fake_stdin" in
  if Sys.file_exists name then Sys.remove name;
  Sys_js.create_file ~name ~content;
  toploop_use_silently ffp name

let execute printval ?pp_code ?highlight_location pp_answer s =
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) pp_code) in
  (try
     while true do
       try
         let phr = !Toploop.parse_toplevel_phrase lb in
         let phr = JsooTopPpx.preprocess_phrase phr in
         ignore (Toploop.execute_phrase printval pp_answer phr : bool)
       with
       | End_of_file -> raise End_of_file
       | x ->
           (match highlight_location with
           | None -> ()
           | Some f -> (
               match JsooTopError.loc x with
               | None -> ()
               | Some loc -> f loc));
           Errors.report_error Format.err_formatter x
     done
   with End_of_file -> ());
  flush_all ()

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Format

module Sig_component_kind = Shape.Sig_component_kind
module String = Misc.Stdlib.String

  type hiding_error =
  | Illegal_shadowing of {
      shadowed_item_id: Ident.t;
      shadowed_item_kind: Sig_component_kind.t;
      shadowed_item_loc: Location.t;
      shadower_id: Ident.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }
  | Appears_in_signature of {
      opened_item_id: Ident.t;
      opened_item_kind: Sig_component_kind.t;
      user_id: Ident.t;
      user_kind: Sig_component_kind.t;
      user_loc: Location.t;
    }

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.explanation
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.explanation
  | With_makes_applicative_functor_ill_typed of
      Longident.t * Path.t * Includemod.explanation
  | With_changes_module_alias of Longident.t * Ident.t * Path.t
  | With_cannot_remove_constrained_type
  | Repeated_name of Sig_component_kind.t * string
  | Non_generalizable of type_expr
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr
  | Recursive_module_require_explicit_type
  | Apply_generative
  | Cannot_scrape_alias of Path.t
  | Cannot_scrape_package_type of Path.t
  | Badly_formed_signature of string * Typedecl.error
  | Cannot_hide_id of hiding_error
  | Invalid_type_subst_rhs
  | Unpackable_local_modtype_subst of Path.t
  | With_cannot_remove_packed_modtype of Path.t * module_type

exception Error of Location.t * Env2.t * error
exception Error_forward of Location.error

open Typedtree

let rec path_concat head p =
  match p with
    Pident tail -> Pdot (Pident head, Ident.name tail)
  | Pdot (pre, s) -> Pdot (path_concat head pre, s)
  | Papply _ -> assert false

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Env2.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | _ -> raise(Error(loc, env, Signature_expected))

let extract_sig_open env loc mty =
  match Env2.scrape_alias env mty with
    Mty_signature sg -> sg
  | Mty_alias path ->
      raise(Error(loc, env, Cannot_scrape_alias path))
  | mty -> raise(Error(loc, env, Structure_expected mty))

(* Compute the environment after opening a module *)

let type_open_ ?used_slot ?toplevel ovf env loc lid =
  Firebug.console##log "typeonnonon";
  Js_of_ocaml.Firebug.console##log "Lookup";
  let path = Env2.lookup_module_path ~load:true ~use:true ~loc:lid.loc lid.txt env in
  Firebug.console##log "OPEN2213123";
  match Env2.open_signature ~loc ?used_slot ?toplevel ovf path env with
  | Ok env -> path, env
  | Error _ ->
      Firebug.console##log "OPENTYPE";
      let md = Env2.find_module path env in
      ignore (extract_sig_open env lid.loc md.md_type);
      assert false


      (* Parsing *)

      let last_token = ref Parser.EOF

      let token lexbuf =
        let token = Lexer.token lexbuf in
        last_token := token;
        token


let rec skip_phrase lexbuf =
  match token lexbuf with
  | Parser.SEMISEMI | Parser.EOF -> ()
  | _ -> skip_phrase lexbuf
  | exception (Lexer.Error (Lexer.Unterminated_comment _, _)
              | Lexer.Error (Lexer.Unterminated_string, _)
              | Lexer.Error (Lexer.Reserved_sequence _, _)
              | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
              | Lexer.Error (Lexer.Illegal_character _, _)) ->
      skip_phrase lexbuf

      let maybe_skip_phrase lexbuf =
        match !last_token with
        | Parser.SEMISEMI | Parser.EOF -> ()
        | _ -> skip_phrase lexbuf

      let wrap parser lexbuf =
        try
          Firebug.console##log "WRAP1";
          Docstrings.init ();
          Firebug.console##log "WRAP2";
          Lexer.init ();
          Firebug.console##log "WRAP3";
          let ast = parser token lexbuf in
          Firebug.console##log "WRAP4";
          Parsing.clear_parser();
          Firebug.console##log "WRAP5";
          Docstrings.warn_bad_docstrings ();
          Firebug.console##log "WRAP6";
          last_token := Parser.EOF;
          ast
        with
        | Lexer.Error(Lexer.Illegal_character _, _) as err
          when String.equal (!Location.input_name) "//toplevel//"->
            skip_phrase lexbuf;
            raise err
        | Syntaxerr.Error _ as err
          when String.equal (!Location.input_name) "//toplevel//" ->
            maybe_skip_phrase lexbuf;
            raise err
        | Parsing.Parse_error | Syntaxerr.Escape_error ->
            let loc = Location.curr lexbuf in
            if (String.equal (!Location.input_name) "//toplevel//")
            then maybe_skip_phrase lexbuf;
            raise(Syntaxerr.Error(Syntaxerr.Other loc))

let simple_module_path = wrap Parser.parse_mod_longident
let initial_env ~loc ~safe_string ~initially_opened_module
  ~open_implicit_modules =
  Firebug.console##log "INIT1";
let env =
  if safe_string then
    Env2.initial_safe_string
  else
    Env2.initial_unsafe_string
in
Firebug.console##log "INIT1";
let open_module env m =
  let open Asttypes in
  Firebug.console##log m;
  let lexbuf = Lexing.from_string m in
  let txt =
    Location.init lexbuf (Printf.sprintf "command line argument: -open %S" m);
    Firebug.console##log "OPEN";
    simple_module_path lexbuf in
    Firebug.console##log "OPE2";
      snd (type_open_ Override env loc {txt;loc})
in
Firebug.console##log "INIT1";
let add_units env units =
  String.Set.fold
    (fun name env ->
        Firebug.console##log (Js.string name);
       Env2.add_persistent_structure (Ident.create_persistent name) env)
    units
    env
in
Firebug.console##log (Load_path.get ());
let units =
  List.map (Load_path.get ()) Env2.persistent_structures_of_dir
in
Firebug.console##log units;
Firebug.console##log "INIT1";
let env, units =
  match initially_opened_module with
  | None -> (env, units)
  | Some m ->
      (* Locate the directory that contains [m], adds the units it
         contains to the environment and open [m] in the resulting
         environment. *)
      Firebug.console##log "LOOP1";
      let rec loop before after =
        match after with
        | [] -> None
        | units :: after ->
            if String.Set.mem m units then
              Some (units, List.rev_append before after)
            else begin
              Firebug.console##log "LOOP2";
              loop (units :: before) after
            end
      in
      let env, units =
        match loop [] units with
        | None ->
            (env, units)
        | Some (units_containing_m, other_units) ->
            (add_units env units_containing_m, other_units)
      in
      Firebug.console##log "LOOP3";
      Firebug.console##log units;
      let v = (open_module env m, units) in
      Firebug.console##log "LOOP4";
      v
in
Firebug.console##log "INIT2";
let env = List.fold_left units add_units env in
Firebug.console##log "INIT3";
let v = List.fold_left open_implicit_modules open_module env in 
Firebug.console##log "INIT4";
v

let initial_env () = 
    Firebug.console##log "ENV1";
    Ident.reinit();
    Firebug.console##log "ENV2";
    Types.Uid.reinit();
    Firebug.console##log "ENV3";
    let initially_opened_module =
      if !Clflags.nopervasives then
        None
      else
        Some "Stdlib"
    in
    Firebug.console##log "ENV4";
    initial_env
      ~loc:(Location.in_file "command line")
      ~safe_string:true (* (Config.safe_string || not !Clflags.uure (nsafe_string) *)
      ~initially_opened_module
      ~open_implicit_modules:(List.rev !Clflags.open_modules)

let initialize () =
  Sys.interactive := false;
  Firebug.console##log (Lazy.force (lazy "Helloo there!"));
  Firebug.console##log (Lazy.force (lazy "Helloo there2!"));
  Lazy.force (lazy (Firebug.console##log (Lazy.force (lazy "Helloo there3!"))));
  Firebug.console##log "SETTOMG ";
  ignore @@ initial_env();
  Toploop.initialize_toplevel_env ();
  Firebug.console##log "SETTOMG2 ";
  Toploop.input_name := "//toplevel//";
  Firebug.console##log "SETTOMG4 ";
  Sys.interactive := true
