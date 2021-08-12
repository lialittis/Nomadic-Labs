open Protocol
open Michelson_v1_primitives

type node = Michelson_value_sampler.node

type location = Michelson_value_sampler.location

val prims_omega : prim array

val get_prim_type : node -> string -> prim

val key_prims_omega : prim array

val const_prims_omega : prim array

val inst_prims_omega : prim array

val stack_man_prims_omega : prim array

val arith_prims_omega : prim array

val control_prims_omega : prim array

val bool_prims_omega : prim array

val data_strt_prims_omega : prim array

val random_select : 'a array -> 'a

module type STRING = sig
  val rand : unit -> string
end

module Mk_String () : STRING

module type INT = sig
  val rand : unit -> Z.t
end

module Mk_Int () : INT

module type PRIM = sig
  val rand : unit -> prim

  val rand_node : location -> node Environment.Error_monad.tzresult Lwt.t

  val simple_rand_node : location -> node Environment.Error_monad.tzresult Lwt.t
end

module Mk_Prim () : PRIM

module type SEQ = sig
  val init : prim -> prim -> node
end

module Mk_Seq () : SEQ

module type MUTATOR = sig
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
    ?additions:int ->
    max_loop:int ->
    max_mutation:int ->
    node ->
    int ->
    node Environment.Error_monad.tzresult Lwt.t
end

module Mk_Mutator () : MUTATOR
