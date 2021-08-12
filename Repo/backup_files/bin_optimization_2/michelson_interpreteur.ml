open Protocol
open Alpha_context
open Script_interpreter
open Michelson_v1_primitives
open Michelson_v1_parser

(* read dir *)

(** [dir_is_empty dir] is true, if [dir] contains no files except
 * "." and ".."
 *)
let dir_is_empty dir = Array.length (Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs ->
        loop (f :: result) fs
    | [] ->
        result
  in
  loop [] [dir]

(* read file functions *)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let read_file_in_1_line filename =
  let result = ref "" in
  let ch = open_in filename in
  try
    while true do
      result := !result ^ input_line ch
    done ;
    !result
  with End_of_file -> close_in ch ; !result

let change_file_format filename =
  let str = read_file_in_1_line filename in
  (*let result = "{parameter unit;storage unit ; code " ^ str ^ "}" in*)
  let result = "{" ^ str ^ "}" in
  print_endline "change the format of script:" ;
  print_endline result ;
  result

(* [TEST] *)
(*
let () = print_endline "############# READ TEST #############"

(*let filename =  "src/bin_optimization/contracts/hello.tz" *)

(*for global running*)
let filename = "./contracts_2/hello.tz"

let test_read_file filename =
  let str = read_file filename in
  print_endline "read script as:" ;
  print_string str

(*test_read_file filename*)

let test_read_file_in_1_line filename =
  let str = read_file_in_1_line filename in
  print_endline "read script in one line:" ;
  print_endline str

(*test_read_file_in_1_line filename*)

let res = change_file_format filename

let () = print_endline "############# END TEST #############"

let () = print_endline ""
*)
(* [ENDTEST] *)

(* print expression to string *)
let print expr : string =
  expr
  |> Micheline_printer.printable (fun s -> s)
  |> Format.asprintf "%a" Micheline_printer.print_expr

(* parser_expression *)
exception Parse_expression

let parse_string str =
  let (ast, errs) = parse_expression ~check:false str in
  ( match errs with
  | [] ->
      Format.printf "parse_expression: success\n" ;
      ()
  | lst ->
      Format.printf "parse_expression: %a\n" Error_monad.pp_print_error lst ;
      raise Parse_expression ) ;
  ast.expanded

(* print any expanded_result *)
let print_expanded expanded_result =
  let str = print (strings_of_prims expanded_result) in
  print_string (str ^ "\n")

(* [TEST]*)
(*
let test_str = "CMPNEQ"

let parsed_string = parse_string test_str

let () = print_expanded parsed_string
*)
(* [ENDTEST] *)

(* Using monad *)
let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error err ->
      (*Lwt.return @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)*)
      Lwt.return @@ Error (Environment.wrap_tztrace err)

let test_context () =
  Context.init 3
  >>=? fun (b, _cs) ->
  Incremental.begin_construction b
  >>=? fun v -> return (Incremental.alpha_ctxt v)

let default_source = Contract.implicit_contract Signature.Public_key_hash.zero

let default_step_constants =
  {
    source = default_source;
    payer = default_source;
    self = default_source;
    amount = Tez.zero;
    chain_id = Chain_id.zero;
  }

(* run_script function;
 * return a binded value after the interpretation *)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  let contract_expr = parse_string contract in
  let storage_expr = parse_string storage in
  let parameter_expr = parse_string parameter in
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false
  >>=?? fun res -> return res

(*test the interpreter*)
let interprete_script filename =
  let context_file = change_file_format filename in
  (*print_string "context file is:";*)
  (*print_string context_file;*)
  test_context ()
  >>=? fun ctx ->
  run_script ctx context_file ~storage:"Unit" ~parameter:"Unit" ()
  >>= function
  | Ok res ->
      Format.printf "test_scipt: success\n" ;
      return @@ res
  | Error errs ->
      Format.printf "test_scipt: fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

(* [TEST]*)
(*
let test_defaut () =
  test_context ()
  >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter unit; storage unit; code {CAR ; NIL operation; PAIR }}"
    ~storage:"Unit"
    ~parameter:"Unit"
    ()
  >>= function
  | Ok _ ->
      Format.printf "test_default_script : success\n" ;
      return_unit
  | Error errs ->
      Format.printf "test_default_script : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let output = test_defaut ()

let test_int () =
  test_context ()
  >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter int; storage int; code { DROP ; PUSH int 1; NEG ; NIL \
     operation ; PAIR}}"
    ~storage:"1"
    ~parameter:"1"
    ()
  >>= function
  | Ok _ ->
      Format.printf "test_int_script : success\n" ;
      return_unit
  | Error errs ->
      Format.printf "test_int_script : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let output2 = test_int ()

(*
;;
List.map
  (fun filename -> interprete_script filename)
  (dir_contents "contracts_2")
*)
*)
(* [ENDTEST] *)
