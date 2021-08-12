open Heuristic_utils
open Heuristics

let of_string (module D : Dist) = (module Mk_simple_search (D) : Solution)

let distance = Heuristics.mk_arith ()

module Simple_search = (val of_string distance : Solution)

let x = Simple_search.gen_sol ()

let () =
  print_string "the final cost is " ;
  print_float x.cost ;
  print_endline ""
