open Protocol
open Michelson_v1_primitives
open Utils_sampler
open Michelson_value_sampler

let prims_omega =
  [| K_parameter;
     K_storage;
     K_code;
     D_False;
     D_Elt;
     D_Left;
     D_None;
     D_Pair;
     D_Right;
     D_Some;
     D_True;
     D_Unit;
     I_PACK;
     I_UNPACK;
     I_BLAKE2B;
     I_SHA256;
     I_SHA512;
     I_ABS;
     I_ADD;
     I_AMOUNT;
     I_AND;
     I_BALANCE;
     I_CAR;
     I_CDR;
     I_CHAIN_ID;
     I_CHECK_SIGNATURE;
     I_COMPARE;
     I_CONCAT;
     I_CONS;
     I_CREATE_ACCOUNT;
     I_CREATE_CONTRACT;
     I_IMPLICIT_ACCOUNT;
     I_DIP;
     I_DROP;
     I_DUP;
     I_EDIV;
     I_EMPTY_BIG_MAP;
     I_EMPTY_MAP;
     I_EMPTY_SET;
     I_EQ;
     I_EXEC;
     I_APPLY;
     I_FAILWITH;
     I_GE;
     I_GET;
     I_GET_AND_UPDATE;
     I_GT;
     I_HASH_KEY;
     I_IF;
     I_IF_CONS;
     I_IF_LEFT;
     I_IF_NONE;
     I_INT;
     I_LAMBDA;
     I_LE;
     I_LEFT;
     I_LEVEL;
     I_LOOP;
     I_LSL;
     I_LSR;
     I_LT;
     I_MAP;
     I_MEM;
     I_MUL;
     I_NEG;
     I_NEQ;
     I_NIL;
     I_NONE;
     I_NOT;
     I_NOW;
     I_OR;
     I_PAIR;
     I_UNPAIR;
     I_PUSH;
     I_RIGHT;
     I_SIZE;
     I_SOME;
     I_SOURCE;
     I_SENDER;
     I_SELF;
     I_SELF_ADDRESS;
     I_SLICE;
     I_STEPS_TO_QUOTA;
     I_SUB;
     I_SWAP;
     I_TRANSFER_TOKENS;
     I_SET_DELEGATE;
     I_UNIT;
     I_UPDATE;
     I_XOR;
     I_ITER;
     I_LOOP_LEFT;
     I_ADDRESS;
     I_CONTRACT;
     I_ISNAT;
     I_CAST;
     I_RENAME;
     I_SAPLING_EMPTY_STATE;
     I_SAPLING_VERIFY_UPDATE;
     I_DIG;
     I_DUG;
     I_NEVER;
     I_VOTING_POWER;
     I_TOTAL_VOTING_POWER;
     I_KECCAK;
     I_SHA3;
     I_PAIRING_CHECK;
     I_TICKET;
     I_READ_TICKET;
     I_SPLIT_TICKET;
     I_JOIN_TICKETS;
     T_bool;
     T_contract;
     T_int;
     T_key;
     T_key_hash;
     T_lambda;
     T_list;
     T_map;
     T_big_map;
     T_nat;
     T_option;
     T_or;
     T_pair;
     T_set;
     T_signature;
     T_string;
     T_bytes;
     T_mutez;
     T_timestamp;
     T_unit;
     T_operation;
     T_address;
     T_sapling_transaction;
     T_sapling_state;
     T_chain_id;
     T_never;
     T_bls12_381_g1;
     T_bls12_381_g2;
     T_bls12_381_fr;
     T_ticket
  |]

let key_prims_omega = [| K_parameter; K_storage; K_code |]

let const_prims_omega =
  [| D_Elt; D_False; D_Left; D_None; D_Pair; D_Right; D_Some; D_True; D_Unit |]

let inst_prims_omega =
  [| I_PACK;
     I_UNPACK;
     I_BLAKE2B;
     I_SHA256;
     I_SHA512;
     I_ABS;
     I_ADD;
     I_AMOUNT;
     I_AND;
     I_BALANCE;
     I_CAR;
     I_CDR;
     I_CHAIN_ID;
     I_CHECK_SIGNATURE;
     I_COMPARE;
     I_CONCAT;
     I_CONS;
     I_CREATE_ACCOUNT;
     I_CREATE_CONTRACT;
     I_IMPLICIT_ACCOUNT;
     I_DIP;
     I_DROP;
     I_DUP;
     I_EDIV;
     I_EMPTY_BIG_MAP;
     I_EMPTY_MAP;
     I_EMPTY_SET;
     I_EQ;
     I_EXEC;
     I_APPLY;
     I_FAILWITH;
     I_GE;
     I_GET;
     I_GET_AND_UPDATE;
     I_GT;
     I_HASH_KEY;
     I_IF;
     I_IF_CONS;
     I_IF_LEFT;
     I_IF_NONE;
     I_INT;
     I_LAMBDA;
     I_LE;
     I_LEFT;
     I_LEVEL;
     I_LOOP;
     I_LSL;
     I_LSR;
     I_LT;
     I_MAP;
     I_MEM;
     I_MUL;
     I_NEG;
     I_NEQ;
     I_NIL;
     I_NONE;
     I_NOT;
     I_NOW;
     I_OR;
     I_PAIR;
     I_UNPAIR;
     I_PUSH;
     I_RIGHT;
     I_SIZE;
     I_SOME;
     I_SOURCE;
     I_SENDER;
     I_SELF;
     I_SELF_ADDRESS;
     I_SLICE;
     I_STEPS_TO_QUOTA;
     I_SUB;
     I_SWAP;
     I_TRANSFER_TOKENS;
     I_SET_DELEGATE;
     I_UNIT;
     I_UPDATE;
     I_XOR;
     I_ITER;
     I_LOOP_LEFT;
     I_ADDRESS;
     I_CONTRACT;
     I_ISNAT;
     I_CAST;
     I_RENAME;
     I_SAPLING_EMPTY_STATE;
     I_SAPLING_VERIFY_UPDATE;
     I_DIG;
     I_DUG;
     I_NEVER;
     I_VOTING_POWER;
     I_TOTAL_VOTING_POWER;
     I_KECCAK;
     I_SHA3;
     I_PAIRING_CHECK;
     I_TICKET;
     I_READ_TICKET;
     I_SPLIT_TICKET;
     I_JOIN_TICKETS
  |]

(*Stack manipulation*)
let stack_man_prims_omega =
  [| I_DIG; I_DIP; I_DROP; I_DUG; I_DUP; I_PUSH; I_SWAP |]

(*Arithmetic operations*)
let arith_prims_omega =
  [| I_ABS;
     I_ADD;
     I_COMPARE;
     I_EDIV;
     I_EQ;
     I_GE;
     I_GT;
     I_INT;
     I_ISNAT;
     I_LE;
     I_LSL;
     I_LSR;
     I_LT;
     I_MUL;
     I_NEG;
     I_NEQ;
     I_SUB
  |]

(*Control structures*)
let control_prims_omega =
  [| I_APPLY;
     I_EXEC;
     I_FAILWITH;
     I_IF;
     I_IF_CONS;
     I_IF_LEFT;
     I_IF_NONE;
     I_ITER;
     I_LAMBDA;
     I_LOOP;
     I_LOOP_LEFT
  |]

(*Boolean operations*)
let bool_prims_omega = [| I_AND; I_OR; I_NOT; I_XOR |]

(*Cryptographic operations*)

(*Blockchain operations*)

(*Operations on data structures*)
let data_strt_prims_omega =
  [| I_CAR;
     I_CDR;
     I_CONCAT;
     I_CONS;
     I_GET;
     I_LEFT;
     I_GET_AND_UPDATE;
     I_MAP;
     I_MEM;
     I_NEVER;
     I_NIL;
     I_NONE;
     I_PACK;
     I_PAIR;
     I_RIGHT;
     I_SIZE;
     I_UNIT;
     I_UNPACK;
     I_UNPAIR;
     I_UPDATE
  |]

(*Operations on tickets*)

let type_prims_omega =
  [| T_bool;
     T_contract;
     T_int;
     T_key;
     T_key_hash;
     T_lambda;
     T_list;
     T_map;
     T_big_map;
     T_nat;
     T_option;
     T_or;
     T_pair;
     T_set;
     T_signature;
     T_string;
     T_bytes;
     T_mutez;
     T_timestamp;
     T_unit;
     T_operation;
     T_address;
     T_sapling_transaction;
     T_sapling_state;
     T_chain_id;
     T_never;
     T_ticket
  |]

let simple_type_prims_omega = [| T_int; T_nat |]

let random_select omega = omega.(Random.int (Array.length omega))

(*
module type ORACLE = sig end
*)

type node = Michelson_value_sampler.node

type location = Michelson_value_sampler.location

(* [TODO] this function is already generated in Rewriting Module*)
(*
let print_node node =
  (* the contract is parsed into node *)
  let str = Michelson_value_sampler.to_string node in
  Printf.printf "The node is : %s\n" str
*)

let get_prim_type (node : node) para_or_sto =
  let index =
    if para_or_sto = "parameter" then 0
    else if para_or_sto = "storage" then 1
    else assert false
  in
  match node with
  | Int _ | String _ | Bytes _ | Prim _ -> assert false
  | Seq (_, nl) -> (
      let x = List.nth nl index in
      match x with
      | None -> assert false
      | Some p -> (
          match p with
          | Micheline.Prim (_, p, nl, _) -> (
              if index = 0 then
                match p with
                | K_parameter -> (
                    let a = List.nth nl 0 in
                    match a with
                    | None -> assert false
                    | Some s -> (
                        match s with
                        | Micheline.Prim (_, p, _, _) -> p
                        | _ -> assert false ) )
                | _ -> assert false
              else
                match p with
                | K_storage -> (
                    let a = List.nth nl 0 in
                    match a with
                    | None -> assert false
                    | Some s -> (
                        match s with
                        | Micheline.Prim (_, p, _, _) -> p
                        | _ -> assert false ) )
                | _ -> assert false )
          | _ -> assert false ) )

(* modules *)
module type STRING = sig
  val rand : unit -> string
end

module Mk_String () : STRING = struct
  let rand () = "test"
end

module type INT = sig
  val rand : unit -> Z.t
end

module Mk_Int () : INT = struct
  let rand () = Z.of_int (Random.int 3) (* 3 is uesd for test *)
end

module type PRIM = sig
  val rand : unit -> prim

  val rand_node : location -> node Environment.Error_monad.tzresult Lwt.t

  val simple_rand_node : location -> node Environment.Error_monad.tzresult Lwt.t

  (*(node, Environment.Error_monad.error Environment.Error_monad.trace) result
    Lwt.t*)
end

module Mk_Prim () : PRIM = struct
  open Micheline

  let rand () = random_select inst_prims_omega

  let rec rand_node location =
    let p = rand () in
    (* the following function may have problem *)
    let rec add_ty_value p =
      Random.self_init () ;
      let ty = random_select type_prims_omega in
      match ty with
      | T_unit | T_int | T_nat | T_bytes | T_bool | T_key_hash | T_timestamp
      | T_mutez ->
          let str_ty = string_of_prim ty in
          gen_node_with_target_type str_ty >>=? fun full_node ->
          let (node, _) = full_node in
          (* let p_cannoical = Micheline.strip_locations node in
              *)
          (* let value = (*how to? and which type ?*) "" in *)
          return
          @@ Prim
               ( location,
                 p,
                 [Prim (location + 1, ty, [], []); node],
                 (*ignore the location for the node*)
                 [] )
      | T_option | T_string | T_pair | T_or | T_set | T_map | T_lambda ->
          (*Lwt.fail_with "ignore for now"*)
          add_ty_value p
      | _ -> add_ty_value p
    in
    match p with
    (* STACK MANIPULATION *)
    | I_DIG | I_DROP | I_DUG | I_DUP ->
        let n = Random.int 2 (*depth*) in
        if n = 0 then return @@ Prim (location, p, [], [])
        else return @@ Prim (location, p, [Int (location + 1, Z.of_int n)], [])
    | I_DIP ->
        let n = Random.int 2 (*depth*) in
        let instr = rand () (*should be specified in some types*) in
        if n = 0 then
          return @@ Prim (location, p, [Prim (location + 1, instr, [], [])], [])
        else
          return
          @@ Prim
               ( location,
                 p,
                 [ Int (location + 1, Z.of_int n);
                   (*why Z ?*)
                   Prim (location + 2, instr, [], []) ],
                 [] )
    | I_PUSH -> add_ty_value p
    (* DATA STRUCTURE *)
    (* ignore the big map class for now *)
    | I_LEFT | I_RIGHT | I_UNPACK -> add_ty_value p
    | I_UPDATE | I_UNPAIR | I_PAIR ->
        let n = Random.int 2 (*depth*) in
        if n = 0 then return @@ Prim (location, p, [], [])
        else return @@ Prim (location, p, [Int (location + 1, Z.of_int n)], [])
    | I_CAR | I_CDR | I_CONCAT | I_CONS | I_PACK | I_SOME | I_UNIT ->
        return @@ Prim (location, p, [], [])
    | _ -> rand_node location

  (* only generate simple random node *)
  let rec simple_rand_node location =
    let p = rand () in
    let rec add_ty_value p =
      Random.self_init () ;
      (* only select type of int or nat *)
      let ty = random_select simple_type_prims_omega in
      match ty with
      | T_unit | T_int | T_nat | T_bytes | T_bool | T_key_hash | T_timestamp
      | T_mutez ->
          let str_ty = string_of_prim ty in
          gen_node_with_target_type str_ty >>=? fun full_node ->
          let (node, _) = full_node in
          (* let p_cannoical = Micheline.strip_locations node in
              *)
          (* let value = (*how to? and which type ?*) "" in *)
          return
          @@ Prim
               ( location,
                 p,
                 [Prim (location + 1, ty, [], []); node],
                 (*ignore the location for the node*)
                 [] )
      | T_option | T_string | T_pair | T_or | T_set | T_map | T_lambda ->
          (*Lwt.fail_with "ignore for now"*)
          add_ty_value p
      | _ -> add_ty_value p
    in
    match p with
    | I_DROP | I_NEG | I_SWAP -> return @@ Prim (location, p, [], [])
    | I_PUSH -> add_ty_value p
    | _ -> simple_rand_node location
end

module type SEQ = sig
  val init : prim -> prim -> node
end

module Mk_Seq () : SEQ = struct
  let init para sto =
    let open Micheline in
    Seq
      ( 0,
        [ Prim (1, K_parameter, [Prim (2, para, [], [])], []);
          Prim (3, K_storage, [Prim (4, sto, [], [])], []);
          Prim
            ( 5,
              K_code,
              [ Seq
                  ( 6,
                    [ Prim (7, I_NIL, [Prim (8, T_operation, [], [])], []);
                      Prim (9, I_PAIR, [], []) ] ) ],
              [] ) ] )
end

module type MUTATOR = sig
  type t = Add | Delete

  type state = Well_Typed | Ill_Typed

  type full_node = { n : node; st : state }

  val self_init_prim : unit -> node

  val init_prim : prim -> prim -> node

  val typecheck :
    node ->
    (Script_tc_errors.type_map * Alpha_context.t)
    Environment.Error_monad.tzresult
    Lwt.t

  (* mutation without typechecking *)
  val mutate : node -> node Environment.Error_monad.tzresult Lwt.t

  (* using typecheck to generate a node *)
  val mutate_2 :
    max_loop:int ->
    node ->
    int ->
    full_node Environment.Error_monad.tzresult Lwt.t
end

module Mk_Mutator () : MUTATOR = struct
  open Script_ir_translator
  open Micheline

  type t = Add | Delete

  type node = Michelson_value_sampler.node

  type state = Well_Typed | Ill_Typed

  type full_node = { n : node; st : state }

  module S = Mk_Seq ()

  module P = Mk_Prim ()

  let of_int = function 0 -> Add | 1 -> Delete | _ -> assert false

  let number_of_types = 2

  let omega = Array.init number_of_types of_int

  let random_select (omega : 'a array) =
    let index = Random.int (Array.length omega) in
    omega.(index)

  let self_init_prim () =
    let para = T_unit in
    let sto = T_unit in
    S.init para sto

  let init_prim para sto = S.init para sto

  let typecheck (node : node) =
    (*gen context*)
    make_context initial_global_gas >>= function
    | Ok ctxt ->
        (*gen expr*)
        let expr = strip_locations node in
        typecheck_code ~legacy:false ctxt expr
    | Error _ -> Lwt.fail_with "Uncorrect context"

  (*monad*)

  let add_op (l : location) (node_list : node list) (target_index : int) =
    P.simple_rand_node (l + 1) >>=? fun n ->
    (*Printf.printf "add_op" ;*)
    let rec add_to_index list index =
      if index = 0 then n :: list
      else
        match list with
        | [] -> assert false
        | hd :: tl -> hd :: add_to_index tl (index - 1)
    in
    let new_nl = add_to_index node_list target_index in
    return @@ Seq (l, new_nl)

  let delete_op (l : location) (node_list : node list) (target_index : int) =
    (* Printf.printf "delete_op" ;*)
    let rec delete_to_index list index =
      if index = 0 then match list with [] -> [] | _ :: tl -> tl
      else
        match list with
        | [] -> assert false
        | hd :: tl -> hd :: delete_to_index tl (index - 1)
    in
    let new_nl = delete_to_index node_list target_index in
    return @@ Seq (l, new_nl)

  let rec mutate (node : node) =
    Random.self_init () ;
    let way = random_select omega in
    match node with
    | Int _ | String _ | Bytes _ -> return node
    | Prim (l, p, nl, a) -> (
        match p with
        | K_parameter -> return node
        | K_storage -> return node
        | K_code -> (
            let seq = List.nth nl 0 in
            match seq with
            | None -> assert false
            | Some s -> (
                match s with
                | Seq (_, _) as s ->
                    mutate s >>=? fun ns -> return @@ Prim (l, p, [ns], a)
                | _ -> assert false ) )
        | _ -> assert false )
    | Seq (l, node_list) ->
        if l = 0 then
          match List.nth node_list 0 with
          | None -> assert false
          | Some para -> (
              match List.nth node_list 1 with
              | None -> assert false
              | Some sto -> (
                  match List.nth node_list 2 with
                  | None -> assert false
                  | Some code ->
                      mutate code >>=? fun ncode ->
                      return @@ Seq (0, [para; sto; ncode]) ) )
        else
          let len = List.length node_list in
          if len = 2 then add_op l node_list 0
          else (
            Random.self_init () ;
            (* len -1 to leave "NIL operation ; PAIR" alone *)
            let target_index = Random.int (len - 2) in
            match way with
            | Add -> add_op l node_list target_index
            | Delete -> delete_op l node_list target_index )

  (* not recursive *)
  let mutate_2 ~(max_loop : int) (node : node) (n : int) =
    if n = max_loop then (
      print_endline
        "##########################################\n\
        \ local search : there is no node qualified\n\
         ##########################################" ;
      return { n = node; st = Ill_Typed } )
    else
      (* method of multiple random mutation : unrecommened*)
      (* let rec loop i t =
           mutate t >>=? fun tmp_t ->
           if i = 1 then return tmp_t else loop (i - 1) tmp_t
         in
         loop add node*)
      mutate node >>=? fun tmp_node ->
      (*let print_node node =
          (* the contract is parsed into node *)
          let str = to_string node in
          Printf.printf "The node iss : %s\n" str
        in
        print_node tmp_node ;*)
      typecheck tmp_node >>= function
      | Ok _ ->
          Printf.printf "Well typed!\n" ;
          return { n = tmp_node; st = Well_Typed }
      | Error _ -> return { n = tmp_node; st = Ill_Typed }

  (*forget about the locatoin for now, we will use other tools to solve this problem*)
end

(* test for mutation *)
(*
let _ =
  let max_try = 500 in
  Random.self_init () ;
  Printf.printf "Test : \n" ;
  let module M = Mk_Mutator () in
  let p = M.self_init_prim () in
  M.mutate_2 p max_try >>= fun x ->
  print_node x ;
  Lwt.return_unit
*)
