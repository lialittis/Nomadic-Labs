module type Dist = sig
  val dist : string -> string -> float

  val is_zero : float -> bool
end

val mk_arith : unit -> (module Dist)

val mk_hamming : unit -> (module Dist)

type temp_sample

module type Sample = sig
  val out_value : unit -> string

  val target_value : unit -> string
end

val of_sample : unit -> (module Sample)

val of_string : (module Dist) -> (module Sample) -> float

(* temp solution type *)
type solution = { tree : Ast.t; cost : float }

val sum_dists :
  ?maxindex:int -> ('a -> 'b -> float) -> 'a array -> 'b array -> float

(* temp solution module *)
module type Solution = sig
  val gen_cost : unit -> Ast.t * float

  val gen_sol : unit -> solution
end

module Mk_simple_search (D : Dist) : Solution
