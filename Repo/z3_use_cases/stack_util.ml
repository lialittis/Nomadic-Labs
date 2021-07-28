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
    if i = length then acc
    else
      let new_x = mk_x i in
      aux (i + 1) (new_x :: acc)
  in
  List.rev (aux 0 [])

(*slice the stack : expr list*)
let slice_stack list start range =
  let rec drop n = function
    | [] -> []
    | _ :: tl as z -> if n = 0 then z else drop (n - 1) tl
  in
  (* need to reverse the list *)
  let rec take n = function
    | [] -> []
    | hd :: tl -> if n = 0 then [] else hd :: take (n - 1) tl
  in
  take range (drop start list)

let idx_to_add alpha = List.range ~start:`inclusive 0 ~stop:`exclusive alpha

let idx_to_del k alpha delta =
  List.range ~start:`inclusive (k - delta + alpha) ~stop:`exclusive k

let idxs_to_presv alpha beta =
  List.range ~start:`inclusive alpha ~stop:`exclusive beta

type t =
  { id : string;
    disasm : string;
    effect : int -> int -> Z3.Expr.expr list -> Z3.Expr.expr list
        (* start position -> index of instruction -> stack -> stack *)
  }

let mk ~id ~effect ~disasm = { id; disasm; effect }

(* only consider integers *)
let mk_PUSH value =
  mk ~id:"PUSH" ~disasm:"PUSH" ~effect:(fun alpha _ current_sk ->
      let range = List.length current_sk - alpha in
      (* int constant *)
      (* let a = mk_a j in*)
      let a = int_with_value value in
      slice_stack current_sk 0 alpha @ (a :: slice_stack current_sk alpha range))

let mk_ADD =
  mk ~id:"ADD" ~disasm:"ADD" ~effect:(fun alpha _ current_sk ->
      let range = List.length current_sk - alpha in
      slice_stack current_sk 0 alpha
      @
      let res = slice_stack current_sk alpha range in
      let presv = slice_stack current_sk (alpha + 2) range in
      let rec aux i res =
        match res with
        | [] -> assert false
        | hd :: tl ->
            if i = 0 then hd
            else
              let i = i - 1 in
              let open Z3Ops in
              hd + aux i tl
      in
      let x' = aux 1 res in
      x' :: presv)

let mk_SWAP =
  mk ~id:"SWAP" ~disasm:"SWAP" ~effect:(fun alpha _ current_sk ->
      let range = List.length current_sk - alpha in
      slice_stack current_sk 0 alpha
      @
      let res = slice_stack current_sk alpha range in
      match res with
      | [] -> []
      | hd :: tl -> (
          match tl with [] -> [hd] | hd' :: tl' -> hd' :: hd :: tl' ))

let mk_DROP =
  mk ~id:"DROP" ~disasm:"DROP" ~effect:(fun alpha _ current_sk ->
      let range = List.length current_sk - alpha in
      slice_stack current_sk 0 alpha
      @
      let res = slice_stack current_sk alpha range in
      match res with [] -> [] | _ :: tl -> tl)

let mk_NEG =
  mk ~id:"NEG" ~disasm:"NEG" ~effect:(fun alpha _ current_sk ->
      let range = List.length current_sk - alpha in
      slice_stack current_sk 0 alpha
      @
      let res = slice_stack current_sk alpha range in
      match res with [] -> [] | _ :: tl -> tl)

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

let ctx = mk_context []

let stact_sort = Z3List.mk_list_s ctx "int_list" int_sort
