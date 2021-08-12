open Protocol
open Sampling_helpers
open Alpha_context

let sampling_parameters =
  let open Michelson_samplers_parameters in
  let size = {Tezos_benchmark.Base_samplers.min = 4; max = 32} in
  {
    int_size = size;
    string_size = size;
    bytes_size = size;
    stack_size = size;
    type_depth = size;
    list_size = size;
    set_size = size;
    map_size = size;
  }

let state = Random.State.make [|42; 987897; 54120|]

module Full = Michelson_samplers_base.Make_full (struct
  let parameters = sampling_parameters

  let algo = `Default

  let size = 16
end)

let int_sampler = Full.Michelson_base.int

type t = Random.State.t

let gen_t = Random.State.make

let example_t = gen_t [|42; 987897; 54120|]

let sampler_t = int_sampler example_t

(* Some Stupid Methods *)
let string_to_type_name = function
  | "string" ->
      `TString
  | "int" ->
      `TInt
  | "list" ->
      `TList
  | "nat" ->
      `TNat
  | _ ->
      Stdlib.failwith "wrong type"

let type_of_atomic_type_name at_tn : Script_ir_translator.ex_ty =
  match at_tn with
  | `TString ->
      Ex_ty (String_t None)
  | `TNat ->
      Ex_ty (Nat_t None)
  | `TKey ->
      Ex_ty (Key_t None)
  | `TBytes ->
      Ex_ty (Bytes_t None)
  | `TBool ->
      Ex_ty (Bool_t None)
  | `TAddress ->
      Ex_ty (Address_t None)
  | `TTimestamp ->
      Ex_ty (Timestamp_t None)
  | `TKey_hash ->
      Ex_ty (Key_hash_t None)
  | `TMutez ->
      Ex_ty (Mutez_t None)
  | `TSignature ->
      Ex_ty (Signature_t None)
  | `TUnit ->
      Ex_ty (Unit_t None)
  | `TInt ->
      Ex_ty (Int_t None)
  | _ ->
      Stdlib.failwith "not expected"

let test_ex_ty = type_of_atomic_type_name (string_to_type_name "string")

let michelson_samplers =
  let module Config = struct
    open Michelson_samplers_parameters

    let parameters =
      {
        int_size = {min = 8; max = 32};
        string_size = {min = 8; max = 128};
        bytes_size = {min = 8; max = 128};
        stack_size = {min = 3; max = 8};
        type_depth = {min = 0; max = 3};
        list_size = {min = 0; max = 1000};
        set_size = {min = 0; max = 1000};
        map_size = {min = 0; max = 1000};
      }

    let size = 16

    let algo = `Default
  end in
  let module Samplers = Michelson_samplers.Make (Config) in
  (module Samplers : Michelson_samplers.S)

module Samplers = (val michelson_samplers)

(*
let target_type = Script_typed_ir.Type_annot global_type_str

let target_type_option =
  match target_type with
  | Script_typed_ir.Type_annot "" ->
      None
  | _ ->
      Some target_type

let foo target_type_option = Gen.sampler
*)

open Sampling_helpers

module Gen : sig
  module Samplers : Michelson_samplers.S

  val sampler : 'a Script_typed_ir.ty -> 'a sampler
end = struct
  module Samplers = Samplers

  let sampler = Samplers.Random_value.value
end

open Script_ir_translator

(*let test_gen (ex_ty : ex_ty) : 'a sampler =
  match ex_ty with
  | Ex_ty a_ty ->
      Gen.sampler a_ty
  | _ ->
      Stdlib.failwith "unexcepted"
*)
(*
type type_name =
  [ `TAddress
  | `TBls12_381_fr
  | `TBls12_381_g1
  | `TBls12_381_g2
  | `TBool
  | `TBytes
  | `TChain_id
  | `TInt
  | `TKey
  | `TKey_hash
  | `TMutez
  | `TNat
  | `TSapling_state
  | `TSapling_transaction
  | `TSignature
  | `TString
  | `TTimestamp
  | `TUnit ]

module Random_type = struct
  let type_of_atomic_type_name (at_tn : type_name) : Script_ir_translator.ex_ty
      =
    match at_tn with
    | `TString ->
        Ex_ty (String_t None)
    | `TNat ->
        Ex_ty (Nat_t None)
    | `TKey ->
        Ex_ty (Key_t None)
    | `TBytes ->
        Ex_ty (Bytes_t None)
    | `TBool ->
        Ex_ty (Bool_t None)
    | `TAddress ->
        Ex_ty (Address_t None)
    | `TTimestamp ->
        Ex_ty (Timestamp_t None)
    | `TKey_hash ->
        Ex_ty (Key_hash_t None)
    | `TMutez ->
        Ex_ty (Mutez_t None)
    | `TSignature ->
        Ex_ty (Signature_t None)
    | `TUnit ->
        Ex_ty (Unit_t None)
    | `TInt ->
        Ex_ty (Int_t None)
    | `TSapling_state ->
        Ex_ty (Sapling_state_t (1, None))
    | `TSapling_transaction ->
        Ex_ty (Sapling_transaction_t (1, None))
    | `TChain_id ->
        Ex_ty (Chain_id_t None)
    | `TBls12_381_g1 ->
        Ex_ty (Bls12_381_g1_t None)
    | `TBls12_381_g2 ->
        Ex_ty (Bls12_381_g2_t None)
    | `TBls12_381_fr ->
        Ex_ty (Bls12_381_fr_t None)
end
*)

type location = int

type node = (location, Script.prim) Micheline.node

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

exception Get_ast

let get_ast str : Script.expr =
  let (ast, errs) = Michelson_v1_parser.parse_expression ~check:false str in
  ( match errs with
  | [] ->
      Format.printf "get_ast: success\n" ;
      ()
  | _ ->
      Format.printf "get_ast: fail\n" ;
      raise Get_ast ) ;
  (*return ast.expanded*)
  ast.expanded

let string = "test"

let node = Micheline.root (get_ast string)

let global_type =
  parse_ty ~allow_big_map:true ~allow_operation:true ~allow_contract:true node

(* ####################################################################################### *)
