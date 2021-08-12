(** Testing
    -------
    Component:    
    Invocation:
    Dependencies:
    Subject:
*)

(*tools*)

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

(*--------------- parser part ---------------------------------------*)

(*print expression to string*)

let print expr : string =
  expr
  |> Micheline_printer.printable (fun s -> s)
  |> Format.asprintf "%a" Micheline_printer.print_expr

(*-------------expanding methods--------------------*)

(* expand function, with argument orginal <- ast
 * 1. get source by print expr function
 * 2. expand the string by expand_all function of Michelson_v1_parser module
 * *)
let expand original =
  let source = print (Micheline.strip_locations original) in
  Michelson_v1_parser.expand_all ~source ~original

(*zero_loc is to define the roof of ast*)
let zero_loc = Micheline_parser.location_zero

open Michelson_v1_parser

(*open Micheline*)
open Protocol
open Michelson_v1_primitives

(*Note that, this module need to open Protocol firstly*)

(* get_expanding_answer function, with argument of string (prim_name)
 * return the expanding_answer by construct a ast prim tree -> original
 * *)
let expanding prim_name =
  let ({expanded = expansion; _}, errors) =
    expand (Prim (zero_loc, prim_name, [], []))
  in
  match errors with
  | [] ->
      let (str : string) = print (strings_of_prims expansion) in
      print_endline str
  | _ ->
      Format.printf "something wrong!\n"

(* test by "COMPNEQ" *)

;;
print_endline "############ PARSER TEST #############"

let test_str = "CMPNEQ"

;;
print_string ("expanding..." ^ test_str ^ "\n")

;;
expanding test_str

(*------------parser by parser_expression-------------*)

exception Get_ast

let get_ast_test str =
  let (ast, errs) = parse_expression ~check:false str in
  ( match errs with
  | [] ->
      Format.printf "get_ast: success\n" ;
      ()
  | lst ->
      Format.printf "get_ast: %a\n" Error_monad.pp_print_error lst ;
      raise Get_ast ) ;
  ast.expanded

(*define the output directly*)
(*return ast.expanded*)
(*here, call return is to define a type converter*)

(*test by a simple string*)

;;
print_endline ("parsering..." ^ test_str)

let output = get_ast_test test_str

(*print function for any expanded_result*)
let print_expanded expanded_result =
  let str = print (strings_of_prims expanded_result) in
  print_string (str ^ "\n")

;;
print_expanded output

;;
print_endline "############ END TEST #############"

(*test on file*)

(*read file*)
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

(*
let test_read =
  (*let storage = "Unit" in*)
  (*let parameter = "Int" in*)
  let script = read_file "./contracts/hardlimit.tz" in
  let script_ast = get_ast_test script in
  script_ast;;

test_read;;
*)

(*---------------------------interpreter part---------------------------------*)

open Alpha_context
open Script_interpreter

(*Return a monad for ast.expanded*)
(*let get_ast str : Script.expr tzresult Lwt.t =*)
let get_ast str : Script.expr =
  let (ast, errs) = parse_expression ~check:false str in
  ( match errs with
  | [] ->
      Format.printf "get_ast: success\n" ;
      ()
  | lst ->
      Format.printf "get_ast: %a\n" Error_monad.pp_print_error lst ;
      raise Get_ast ) ;
  (*return ast.expanded*)
  ast.expanded

(*here, call return is to define a type converter*)

(*define the >>= function*)
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

(*
module Logger : STEP_LOGGER = struct
  let log_interp _ctxt _descr _stack = ()

  let log_entry _ctxt _descr _stack = ()

  let log_exit _ctxt _descr _stack = ()

  let get_log () = Lwt.return (Ok None)
end


let run_step ctxt code param =
  Script_interpreter.step
    (module Logger)
    ctxt
    default_step_constants
    code
    param
*)

(*
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  get_ast contract
  >>=? fun contract_expr ->
  get_ast storage
  >>=? fun storage_expr ->
  get_ast parameter
  >>=? fun parameter_expr ->
  let script =
    Script.{code = lazy_expr contract_expr; storage = lazy_expr storage_expr}
  in
  (Script_interpreter.execute
    ctx
    Readable
    step_constants
    ~script
    ~entrypoint
    ~parameter:parameter_expr
    ~internal:false)
  >>=?? fun res -> return res
*)

(*run_script function; return a monad after the interpretation*)
let run_script ctx ?(step_constants = default_step_constants) contract
    ?(entrypoint = "default") ~storage ~parameter () =
  let contract_expr = get_ast contract in
  let storage_expr = get_ast storage in
  let parameter_expr = get_ast parameter in
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

(*test the read function: bugs not fixed*)

;;
print_endline "############# READ TEST #############"

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

let change_file_format filename =
  let str = read_file_in_1_line filename in
  (*let result = "{parameter unit;storage unit ; code " ^ str ^ "}" in*)
  let result = "{" ^ str ^ "}" in
  print_endline "change the format of script:" ;
  print_endline result ;
  result

;;
change_file_format filename

;;
print_endline "############# END TEST #############"

;;
print_endline ""

(*test the interpreter*)
let test_script filename =
  let context_file = change_file_format filename in
  (*print_string "context file is:";*)
  (*print_string context_file;*)
  test_context ()
  >>=? fun ctx ->
  run_script ctx context_file ~storage:"Unit" ~parameter:"Unit" ()
  >>= function
  | Ok _ ->
      Format.printf "test_scipt: success\n" ;
      return_unit
  | Error errs ->
      Format.printf "test_scipt: fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

;;
List.map (fun filename -> test_script filename) (dir_contents "contracts_2")

(*TEST*)
