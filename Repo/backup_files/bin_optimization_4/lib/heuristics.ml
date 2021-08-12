module type Dist = sig
  val dist : string -> string -> float

  val is_zero : float -> bool
end

(* To make distance computing methods *)
let mk_arith () =
  ( module struct
    let dist x y = Stdlib.abs_float (Float.of_string x -. Float.of_string y)

    let is_zero x = abs_float x < 0.5
  end : Dist )

let mk_hamming () =
  ( module struct
    exception Length_Diff

    let dist x y =
      if String.length x != String.length y then raise Length_Diff
      else
        let len = String.length x in
        let rec aux dist i len =
          if i = len then dist
          else if x.[i] == y.[i] then aux dist (i + 1) len
          else aux (dist + 1) (i + 1) len
        in
        Float.of_int (aux 0 0 len)

    let is_zero x = abs_float x < 1.
  end : Dist )

(* temperary sample type *)
type sample = { out_value : string; target_value : string }

module type Sample = sig
  val out_value : unit -> string

  val target_value : unit -> string
end

let of_out_value s = s.out_value

let of_target_value s = s.target_value

(* first-class module of Sample *)
let of_sample () =
  ( module struct
    let sample = { out_value = "out_value"; target_value = "target_value" }

    let out_value () = of_out_value sample

    let target_value () = of_target_value sample
  end : Sample )

(* using first-classes to calculate distance in float *)
let of_string (module D : Dist) (module S : Sample) =
  let x = S.out_value () in
  let y = S.target_value () in
  let distance = D.dist x y in
  distance

(* test *)
(*
let ham_dis_test = mk_hamming ()

let ari_dis_test = mk_arith ()

let sam_test = of_sample ()

let res_test = of_string ham_dis_test sam_test

let res_test = of_string ari_dis_test sam_test
*)

(* in case that there are multiple distances *)
let sum_dists ?maxindex dist arr arr' =
  let max = match maxindex with Some i -> i | None -> Array.length arr in
  let rec aux i =
    if i < max then dist arr.(i) arr'.(i) +. aux (i + 1) else 0.
  in
  aux 0

(* temp solution type *)
type solution = { tree : Ast.t; cost : float }

(* temp solution module *)
module type Solution = sig
  val gen_cost : unit -> Ast.t * float

  val gen_sol : unit -> solution
end

(* functor of module Solution *)
module Mk_simple_search (D : Dist) : Solution = struct
  (* to end the loop *)
  let is_stuck n = n > 20

  module M = Ast.Mk_Unop ()

  module I = Ast.Mk_Input ()

  module T = Ast.Mk_Template ()

  (* Actually the cost here stand for the distance between the expected output and actual output *)
  (*
  let gen_cost () =
    Random.self_init () ;
    (* necessary *)
    let op = M.rand () in
    let actual_out = string_of_int (M.apply op I.input) in
    let exp_out = string_of_int I.output in
    let cost = D.dist actual_out exp_out in
    print_string "cost is" ;
    print_float cost ;
    print_endline "" ;
    cost
*)

  let gen_cost () =
    Random.self_init () ;
    (* necessary *)
    let t = T.random_unop () in
    let vals = T.get_values_from_t t in
    let actual_outs = Array.map string_of_int vals in
    let exp_outs =
      Array.map string_of_int [| snd Ast.Oracle.sample_samples.out |]
    in
    let cost = sum_dists D.dist actual_outs exp_outs in
    print_string "cost: " ;
    print_float cost ;
    print_endline "" ;
    print_endline "-------" ;
    (t, cost)

  (* generate solution based on the cost and tree *)
  (* for now, forget the tree *)
  (* let gen_sol cost = { cost }*)

  (* loop/recursion *)
  let rec loop t cost n =
    if is_stuck n || D.is_zero cost then (t, cost)
    else (
      print_string "Run : " ;
      print_int n ;
      print_endline "" ;
      let (t', cost') = gen_cost () in
      loop t' cost' (n + 1) )

  (* to generate the solution; now only the cost *)
  let gen_sol () =
    let (t, cost) = gen_cost () in
    let (t', cost') = loop t cost 0 in
    { tree = t'; cost = cost' }
end

(*
let of_string (module D : Dist) = (module Mk_simple_search (D) : Solution)
*)
