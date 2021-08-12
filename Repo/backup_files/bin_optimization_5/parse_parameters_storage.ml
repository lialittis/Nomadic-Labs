open Protocol
open Alpha_context
open Utils_sampler
open Michelson_value_sampler
open Script_ir_translator

(*-----------------First Step---------------*)

(* parse the input script to Micheline.node *)

let script_string_from_file filename =
  let res = change_file_format filename in
  res

(* you should not use parse_expression, which generate Micheline_parse.node
 * but not (location, Script.prim) Micheline.node *)
let script_node_from_file filename =
  let str = script_string_from_file filename in
  let node = gen_node str in
  node

(* generate code: lazy_expr *)
let gen_code contract =
  let contract_expr = Michelson_interpreteur.parse_string contract in
  let code = Script.lazy_expr contract_expr in
  code

(* parse_code to get type of storage and parameter , ignore type_logger *)
let parse_code_expr code ~legacy =
  gen_unimportant_context () >>= fun res ->
  match res with
  | Ok ctxt -> parse_code ctxt ~code ~legacy
  | Error errs ->
      Format.printf "parsed code wrong !\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let gen_para_ty code ~legacy =
  parse_code_expr code ~legacy >>=? fun (Ex_code { arg_type; _ }, ctxt) ->
  return (Ex_ty arg_type, ctxt)

let gen_storage_ty code ~legacy =
  parse_code_expr code ~legacy >>=? fun (Ex_code { storage_type; _ }, ctxt) ->
  return (Ex_ty storage_type, ctxt)

let gen_para_storage_ty code ~legacy =
  parse_code_expr code ~legacy
  >>=? fun (Ex_code { arg_type; storage_type; _ }, ctxt) ->
  return ((Ex_ty arg_type, Ex_ty storage_type), ctxt)

(*
  >>=? fun (Ex_code {code;arg_type;storage_type;root_name},ctxt) ->
  parse_storage
    ?type_logger
    ctxt
    ~legacy
    ~allow_forged:allow_forged_in_storage
    storage_type
    ~storage
  >|=? fun (storage, ctxt) ->
  (Ex_script {code; arg_type; storage; storage_type; root_name}, ctxt)

(* parse the node and get the parameter in type ex_ty *)
let gen_parameter node =
  gen_unimportant_context ()
  >>=? fun ctx ->
  let t = parse_parameter_ty ctx ~legacy:false node in
  Lwt.return @@ t >>=?? fun res -> return res
*)

let gen_input_exvalue_from_filename filename ~gen_exty ~legacy ~argument =
  let contract = script_string_from_file filename in
  let code = gen_code contract in
  gen_exty code ~legacy >>=? fun (ex_ty, ctxt) ->
  print_string ("Generate " ^ argument ^ " as exvalue successfully: ") ;
  return @@ (gen_exvalue ex_ty, ctxt)

(*
let gen_para_exvalue_from_filename filename ~legacy =
  let contract = script_string_from_file filename in
  let code = gen_code contract in
  gen_para_ty ~legacy code
  >>=? fun (ex_ty, ctxt) ->
  Printf.printf "gen parameter as exvalue successfully:" ;
  return @@ (gen_exvalue ex_ty, ctxt)

(* TOFIX there is a bug : we don't need to sample the storage *)

let gen_storage_exvalue_from_filename filename ~legacy =
  let contract = script_string_from_file filename in
  let code = gen_code contract in
  gen_storage_ty ~legacy code
  >>=? fun (ex_ty, ctxt) ->
  Printf.printf "gen storage as exvalue successfully:" ;
  return @@ (gen_exvalue ex_ty, ctxt)
*)

let gen_para_storage_exvalue filename ~legacy =
  let contract = script_string_from_file filename in
  let code = gen_code contract in
  gen_para_storage_ty ~legacy code >>=? fun ((para_ty, stor_ty), ctxt) ->
  return @@ ((gen_exvalue para_ty, ctxt), (gen_exvalue stor_ty, ctxt))

(*
  match ex_ty with
  | Ok t ->
      let (exty, ctxt) = t in
      let exvalue = gen_exvalue exty in
      return @@ (exvalue, ctxt)
  | Error errs ->
      Format.printf "wrong !\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
*)

(* Lwt.return @@ Error (Environment.wrap_tztrace errs)
 * this could return Error_monad find this method in >>=?? function*)

let gen_full_node_from_exvalue res =
  let (exvalue, ctxt) = res in
  match exvalue with
  | Ex_Value (a_ty, a) ->
      let node = unparse_data ctxt Readable a_ty a in
      node

let gen_full_node_from_file retract_function filename ~legacy =
  retract_function filename ~legacy >>=? fun res ->
  gen_full_node_from_exvalue res

(* [TEST] *)
(*
let test_file = "simple_contract.tz"
*)
(* [ENDTEST] *)

(* print function, print string to check if we get the correct parameter type *)

let print_prims retract_function filename =
  gen_full_node_from_file retract_function filename ~legacy:true
  >>=?? fun full_node ->
  let (node, _) = full_node in
  let p_canonical = Micheline.strip_locations node in
  let str = print (Michelson_v1_primitives.strings_of_prims p_canonical) in
  print_endline str ;
  return @@ str

let get_para =
  print_prims
    (gen_input_exvalue_from_filename
       ~gen_exty:gen_para_ty
       ~argument:"parameter")

let get_storage =
  print_prims
    (gen_input_exvalue_from_filename
       ~gen_exty:gen_storage_ty
       ~argument:"storage")

(*
let para_as_string = get_para test_file

let storage_as_string = get_storage test_file
*)
