open Core
open Z3util
open Z3

(*open Consts*)

let mk_x (i : int) = Z3util.intconst ("x_" ^ string_of_int i)

(* argument a of instruction after j instructions *)
let mk_a (j : int) = Z3util.intconst ("a_" ^ string_of_int j)

(* make initial stack *)
let sk_init ~length =
  let rec aux i acc =
    if i < 0 then acc else aux (i - 1) (mk_cons [mk_x i; acc])
  in
  aux (length - 1) (Z3List.nil stack_sort)

(* only consider integers *)
let mk_PUSH current_sk value sk_count index =
  let n = num value in
  let expr = mk_cons [n; current_sk] in
  let mk_s =
    func_decl
      ("s_" ^ string_of_int sk_count ^ "_" ^ string_of_int index)
      [stack_sort]
      stack_sort
  in
  FuncDecl.apply mk_s [expr]

let mk_ADD current_sk sk_count index =
  let hd = get_hd [current_sk] in
  let hd' = get_hd [get_tl [current_sk]] in
  let open Z3Ops in
  let expr = mk_cons [hd + hd'; get_tl [get_tl [current_sk]]] in
  let mk_s =
    func_decl
      ("s_" ^ string_of_int sk_count ^ "_" ^ string_of_int index)
      [stack_sort]
      stack_sort
  in
  FuncDecl.apply mk_s [expr]

let mk_SWAP current_sk sk_count index =
  let hd = get_hd [current_sk] in
  let hd' = get_hd [get_tl [current_sk]] in
  let tl = get_tl [get_tl [current_sk]] in
  let expr = mk_cons [hd'; mk_cons [hd; tl]] in
  let mk_s =
    func_decl
      ("s_" ^ string_of_int sk_count ^ "_" ^ string_of_int index)
      [stack_sort]
      stack_sort
  in
  FuncDecl.apply mk_s [expr]

let mk_DROP current_sk sk_count index =
  let expr = get_tl [current_sk] in
  let mk_s =
    func_decl
      ("s_" ^ string_of_int sk_count ^ "_" ^ string_of_int index)
      [stack_sort]
      stack_sort
  in
  FuncDecl.apply mk_s [expr]

(*
type t = { id : string; disasm : string; effect : int -> int -> Z3.Expr.expr }

let mk ~id ~effect ~disasm = { id; disasm; effect }

let enc_semtc ~in_ws ~out_ws ~alpha ~delta k j =
  let out_idxs =
    List.range ~start:`inclusive (k - alpha + delta) ~stop:`exclusive k
  in
  let x i = mk_x i j and x' i = mk_x' i j in
  let u i = mk_u i j in
  (* let in_ws' = if is_commutative then permutations in_ws else [in_ws] in
  *)
  let open Z3Ops in
  conj (List.mapi in_ws ~f:(fun i _ -> u i))
  && disj
       (List.map [in_ws] ~f:(fun ws ->
            conj (List.mapi ws ~f:(fun i w -> x i == w))))
  && conj (List.mapi out_ws ~f:(fun i w -> x' i == w))
  && conj (List.map out_idxs ~f:(fun i -> ~!(u i)))

let enc_userdef ~in_ws ~out_ws k j =
  let delta = List.length in_ws and alpha = List.length out_ws in
  let open Z3Ops in
  enc_semtc ~in_ws ~out_ws ~alpha ~delta k j
  && enc_prsv k j ~alpha ~delta
  && enc_sk_utlz k j ~alpha ~delta

let mk_PUSH =
  mk ~id:"PUSH" ~disasm:"PUSH" ~effect:(fun j current_sk ->
      let a = mk_a j in
      enc_userdef ~in_ws:[] ~out_ws:[a] k j)
*)

(*
let _ =
  let i = intsymbol 1 in
  let s = symbol_to_string i in
  Printf.printf "1 symbol is :%s\n" s ;
  let ar = sort_list "test test" int_sort in
  let str = Sort.to_string ar in
  Printf.printf "sort is %s\n" str ;
  let fun_decl = Z3List.get_head_decl ar in
  let str = FuncDecl.to_string fun_decl in
  print_string str ;
  print_endline "//" ;
  let stack_init = sk_init ~length:10 in
  (* List.map stack_init ~f:(fun v -> print_endline (Z3.Expr.to_string v)) ;*)
  let push = mk_PUSH 1 in
  let add = mk_ADD in
  let drop = mk_DROP in
  let stack = push.effect 0 1 stack_init in
  let stack = add.effect 0 2 stack in
  let stack = drop.effect 2 3 stack in
  List.map stack ~f:(fun v -> print_endline (Z3.Expr.to_string v))
*)
