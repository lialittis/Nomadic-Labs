open Protocol
open Alpha_context

(* open Script_interpreter *)

type 'a ty = 'a Script_typed_ir.ty

type ex_ty = Script_ir_translator.ex_ty

type ex_value = Ex_Value : 'a ty * 'a -> ex_value

(* [TEST]*)
(*
let test_type_string : global_type_string = "int"

(* Input parameter parsing *)
let _ =
  if Array.length Sys.argv < 3 then (
    Format.eprintf
      "Executable expects random seed and target type on input\n%!" ;
    exit 1 )
  else Random.init (int_of_string Sys.argv.(1))

let test_type_string = Sys.argv.(2)
*)
(* [ENDTEST] *)

(*------------------------------First Step--------------------------------------*)

(******************************string -> node********************************)

(* Step1 : get the ast after expanded *)
exception Get_ast

(* string -> Scipt.expr : Michelson_v1_primitives.prim canonical*)
let get_ast str : Script.expr =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:false str in
  ( match errs with
  | [] ->
      (*Format.printf "get_ast: success\n" ;*)
      ()
  | _ ->
      Format.printf "get_ast: fail\n" ;
      raise Get_ast ) ;
  (*return ast.expanded*)
  ast.expanded

(* Step2 : get the Micheline.node without expand_all
   -> (location, string) Micheline.node *)
let parse_expression ?check source =
  let (tokens, _errs) = Micheline_parser.tokenize source in
  let (ast, _par_errs) = Micheline_parser.parse_expression ?check tokens in
  ast

(* [TEST]*)
(*
(* node_parser is Micheline_parse.node : (Micheline_parser.location, string) Micheline.node *)
let node_parser = parse_expression ~check:false test_type_string

(* node : (int , Micheline_v1_primitives.prim) Micheline.node*)
let node = Micheline.root (get_ast test_type_string)
*)
(* [ENDTEST]*)

type location = int

type node = (location, Script.prim) Micheline.node

let gen_node str = Micheline.root (get_ast str)

(******************Micheline.node -> ex_ty*******************************)

(* pretty printing formatter *)
let pp fmt node =
  let canonical = Micheline.strip_locations node in
  let printable =
    Micheline_printer.printable
      Michelson_v1_primitives.string_of_prim
      canonical
  in
  Micheline_printer.print_expr fmt printable

let to_string node =
  pp Format.str_formatter node ;
  Format.flush_str_formatter ()

(* Method of node -> ex_ty directly *)

(*

let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error err ->
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

let parse_ty =
  Script_ir_translator.parse_ty >|?= fun n_lwt -> Lwt.return @@ n_lwt

let parse_ty = Script_ir_translator.parse_ty

let gen_exty target_type =
  let node = gen_node target_type in
  test_context ()
  >>? fun ctx ->
  parse_ty
    ctx
    ~legacy:true
    ~allow_lazy_storage:true
    ~allow_operation:true
    ~allow_contract:true
    ~allow_ticket:true
    node
  >>= function
  | Ok _ ->
      Format.printf "string -> exty : success\n"
  | Error errs ->
      Format.printf "string -> exty : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs
*)

(* Method of node -> type.base.t -> ex_ty *)

(* Adapted from Script_ir_translator.parse_ty *)
let rec parse_ty :
    allow_big_map:bool ->
    allow_operation:bool ->
    allow_contract:bool ->
    node ->
    Type.Base.t =
 fun ~allow_big_map ~allow_operation ~allow_contract node ->
  match node with
  | Prim (_loc, T_unit, [], _annot) ->
      Type.unit
  | Prim (_loc, T_int, [], _annot) ->
      Type.int
  | Prim (_loc, T_nat, [], _annot) ->
      Type.nat
  | Prim (_loc, T_string, [], _annot) ->
      Type.string
  | Prim (_loc, T_bytes, [], _annot) ->
      Type.bytes
  | Prim (_loc, T_bool, [], _annot) ->
      Type.bool
  | Prim (_loc, T_key_hash, [], _annot) ->
      Type.key_hash
  | Prim (_loc, T_timestamp, [], _annot) ->
      Type.timestamp
  | Prim (_loc, T_mutez, [], _annot) ->
      Type.mutez
  | Prim (_loc, T_option, [ut], _annot) ->
      let ty = parse_ty ~allow_big_map ~allow_operation ~allow_contract ut in
      Type.option ty
  | Prim (_loc, T_pair, [utl; utr], _annot) ->
      let lty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utl in
      let rty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utr in
      Type.pair lty rty
  | Prim (_loc, T_or, [utl; utr], _annot) ->
      let lty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utl in
      let rty = parse_ty ~allow_big_map ~allow_operation ~allow_contract utr in
      Type.union lty rty
  | Prim (_loc, T_set, [ut], _annot) ->
      let ut = parse_ty ~allow_big_map ~allow_operation ~allow_contract ut in
      Type.set ut
  | Prim (_loc, T_map, [uta; utb], _annot) ->
      let uta = parse_ty ~allow_big_map ~allow_operation ~allow_contract uta in
      let utb = parse_ty ~allow_big_map ~allow_operation ~allow_contract utb in
      Type.map uta utb
  | Prim (_loc, T_lambda, [dom; range], _annot) ->
      let dom = parse_ty ~allow_big_map ~allow_operation ~allow_contract dom in
      let range =
        parse_ty ~allow_big_map ~allow_operation ~allow_contract range
      in
      Type.lambda dom range
  | Prim (_loc, T_list, [elt], _annot) ->
      let elt = parse_ty ~allow_big_map ~allow_operation ~allow_contract elt in
      Type.list elt
  | _ ->
      let s = to_string node in
      Stdlib.failwith ("Mikhailsky.parse_ty: could not parse " ^ s)

(* [TEST] *)
(*
let target_type =
  parse_ty ~allow_big_map:true ~allow_operation:true ~allow_contract:true node
*)
(* [ENDTEST] *)
open Test_helpers

let gen_exty ty =
  Context.init 3
  >>=? fun (b, _cs) ->
  Incremental.begin_construction b
  >>=? fun v ->
  let ctxt = Incremental.alpha_ctxt v in
  let exty = base_type_to_ex_ty ty ctxt in
  return @@ exty

(* [TEST] *)
(*
let exty = gen_exty target_type
*)
(* [ENDTEST] *)

(*------------------------------------Second Step-----------------------------------------*)

(***************************michelson sampler : 'a ty -> 'a sampler*******************************)
let michelson_samplers =
  let module Config = struct
    open Michelson_samplers_parameters

    let parameters =
      {
        int_size = {min = 8; max = 16};
        string_size = {min = 8; max = 16};
        bytes_size = {min = 8; max = 16};
        stack_size = {min = 3; max = 8};
        type_depth = {min = 0; max = 3};
        list_size = {min = 0; max = 100};
        set_size = {min = 0; max = 100};
        map_size = {min = 0; max = 100};
      }

    let size = 16

    let algo = `Default
  end in
  let module Samplers = Michelson_samplers.Make (Config) in
  (module Samplers : Michelson_samplers.S)

module Samplers = (val michelson_samplers)

let state = Random.State.make_self_init ()

open Sampling_helpers

module Gen : sig
  module Samplers : Michelson_samplers.S

  val rng_state : Random.State.t

  val sampler : 'a Script_typed_ir.ty -> 'a sampler
end = struct
  module Samplers = Samplers

  let rng_state = state

  let sampler = Samplers.Random_value.value
end

(**************************ex_ty -> ex_value*********************************)

let gen a_ty =
  let open Gen in
  let res = (sampler a_ty) rng_state in
  res

let gen_exvalue exty =
  match exty with Script_ir_translator.Ex_ty ty -> Ex_Value (ty, gen ty)

(*
type 'a ty (*GADT ?*) done

val gen : 'a ty -> 'a = <fun> done

type exty = Exty of 'a ty (* or GADT *) done

type exvalue = ExValue : 'a ty * 'a -> exvalue done

val f : exty -> exvalue = <fun> done

val random_value : 'a ty -> 'a sampler done

type 'a sampler = Random.State.t -> 'a done

gen = randome_value + 'a sampler Random.State.t done

val Script_ir_translator.parse_ty : context -> ... -> node -> exty (not using this one, but done)
*)

(************************String input to exvalue**************************************)

let string_input_to_exvalue global_type_string =
  let node = gen_node global_type_string in
  let target_type =
    parse_ty
      ~allow_big_map:true
      ~allow_operation:true
      ~allow_contract:true
      node
  in
  gen_exty target_type
  >>= fun exty ->
  match exty with
  | Ok t ->
      Format.printf "string convert to exvalue : success\n" ;
      let (exty, context) = t in
      let exvalue = gen_exvalue exty in
      return @@ (exvalue, context)
  | Error errs ->
      Format.printf "string convert to exvalue : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

(*----------------------------------------Third Step----------------------------------------*)

(******************************unparse data*******************************************)
let unparse_data = Script_ir_translator.unparse_data

let gen_node_with_target_type global_type_string =
  string_input_to_exvalue global_type_string
  >>=? fun res ->
  let (exv, ctxt) = res in
  match exv with
  | Ex_Value (a_ty, a) ->
      let node = unparse_data ctxt Readable a_ty a in
      node

(* [TEST] *)
(*
let full_node = gen_node_with_target_type test_type_string
*)
(* [ENDTEST] *)
(******************************print data**********************************************)
(*print expression to string*)
let print expr : string =
  expr
  |> Micheline_printer.printable (fun s -> s)
  |> Format.asprintf "%a" Micheline_printer.print_expr

let print_prims global_type_string =
  gen_node_with_target_type global_type_string
  >>=? fun full_node ->
  let (node, _) = full_node in
  let p_canonical = Micheline.strip_locations node in
  let str = print (Michelson_v1_primitives.strings_of_prims p_canonical) in
  print_endline str ; return @@ str

(* [TEST] *)
(*
let output_as_string = print_prims test_type_string
*)
(* [ENDTEST] *)
