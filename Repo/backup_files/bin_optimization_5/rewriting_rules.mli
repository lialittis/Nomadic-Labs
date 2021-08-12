open Protocol
open Sampling
open Heuristic_utils.Heuristics

type node

module type RULES = sig
  type t = node

  val check_types_2 : t -> bool

  val swap_1 : t -> int list

  val cut : t -> int -> int list -> t

  val gen_init_node : string -> t Environment.Error_monad.tzresult Lwt.t

  val interprete_random_node :
    sample array -> t -> Alpha_context.t -> int -> (string array * string) Lwt.t

  type sol = { node : t; cost : string; distance : float }

  val distance_list : float list ref

  val process :
    string -> string -> int -> sol list Environment.Error_monad.tzresult Lwt.t
end

val mk_rules : (module Dist) -> (module RULES)

val rewrite_random : string -> string -> int -> (module RULES) -> unit Lwt.t

val simplify : string -> (module RULES) -> string
