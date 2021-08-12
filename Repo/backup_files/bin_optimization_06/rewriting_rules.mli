open Protocol
open Sampling
open Heuristic_utils.Heuristics
open Tree

type node

module type RULES = sig
  type t = node

  module M : MUTATOR

  val check_types_2 : t -> bool

  val swap_1 : t -> int list

  val cut : t -> int -> int list -> t

  val gen_init_node :
    string -> M.full_node Environment.Error_monad.tzresult Lwt.t

  val interprete_random_node :
    sample array -> t -> Alpha_context.t -> int -> (string array * string) Lwt.t

  type sol = { full_node : M.full_node; cost : string; distance : float }

  val distance_list : float list ref

  val process :
    string -> string -> int -> sol list Environment.Error_monad.tzresult Lwt.t
end

val mk_rules : (module Dist) -> (module RULES)

val rewrite_random : string -> string -> int -> (module RULES) -> unit Lwt.t

val simplify : string -> (module RULES) -> string
