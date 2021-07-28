(*   Copyright 2020 Maria A Schett

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
open Z3util
open Consts
open Sk_util
open Core

(*
let rec permutations xs0 =
  let rec perms xs is =
    match xs with
    | [] -> []
    | t :: ts ->
        let rec interleave' f xs' r =
          match xs' with
          | [] -> (ts, r)
          | y :: ys ->
              let (us, zs) = interleave' (fun xs -> f (List.cons y xs)) ys r in
              (y :: us, f (t :: y :: us) :: zs)
        in
        let interleave xs r =
          let (_, zs) = interleave' Fn.id xs r in
          zs
        in
        List.fold_right
          (permutations is)
          ~f:interleave
          ~init:(perms ts (t :: is))
  in
  xs0 :: perms xs0 []
*)

type t = { id : string; disasm : string; effect : int -> int -> Z3.Expr.expr }
[@@deriving show { with_path = false }]

let mk ~id ~effect ~disasm = { id; disasm; effect }

(* show instruction *)

let is_PUSH iota = String.equal iota.id "PUSH"

let show_hex arg =
  let hx = Z.format "x" arg in
  if Int.rem (String.length hx) 2 = 1 then "0" ^ hx else hx

let compute_idx arg = String.length (show_hex arg) / 2

let show_disasm ?(arg = None) iota =
  let idx =
    if is_PUSH iota then [%show: int] (compute_idx (Option.value_exn arg))
    else ""
  in
  iota.disasm ^ idx
  ^ Option.value_map arg ~default:"" ~f:(fun i -> " " ^ Z.to_string i)

(*
let hex_add base idx =
  Z.add (Z.of_string_base 16 base) (Z.of_int idx) |> Z.format "x"

let show_opcode ?(arg = None) iota =
  if is_PUSH iota then
    let arg = Option.value_exn arg in
    let idx = compute_idx arg in
    hex_add iota.opcode (idx - 1) ^ show_hex arg
  else iota.opcode

let get_gas iota = iota.gas
*)

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

let mk_userdef id ~in_ws ~out_ws ~disasm =
  mk ~id ~effect:(enc_userdef ~in_ws ~out_ws) ~disasm

(* predefined instructions *)

let mk_PUSH =
  mk ~id:"PUSH" ~disasm:"PUSH" ~effect:(fun k j ->
      let a = mk_a j in
      enc_userdef ~in_ws:[] ~out_ws:[a] k j)

let mk_DROP =
  mk ~id:"DROP" ~disasm:"DROP" ~effect:(fun k j ->
      let x_0 = mk_x 0 j in
      enc_userdef ~in_ws:[x_0] ~out_ws:[] k j)

let mk_SWAP idx =
  let disasm = "SWAP" ^ [%show: int] idx in
  mk ~id:disasm ~disasm ~effect:(fun k j ->
      let x_0 = mk_x 0 j and x_l = mk_x idx j in
      let prsvd =
        List.map
          ~f:(fun i -> mk_x i j)
          (List.range ~start:`inclusive 1 ~stop:`exclusive idx)
      in
      enc_userdef
        ~in_ws:([x_0] @ prsvd @ [x_l])
        ~out_ws:([x_l] @ prsvd @ [x_0])
        k
        j)

let mk_DUP idx =
  let disasm = "DUP" ^ [%show: int] idx in
  mk ~id:disasm ~disasm ~effect:(fun k j ->
      let x_idx = mk_x (idx - 1) j in
      let prsvd =
        List.map
          ~f:(fun i -> mk_x i j)
          (List.range ~start:`inclusive 0 ~stop:`exclusive (idx - 1))
      in
      enc_userdef
        ~in_ws:(prsvd @ [x_idx])
        ~out_ws:([x_idx] @ prsvd @ [x_idx])
        k
        j)

(*
let predef ~k =
  let max_idx = Int.min (k - 1) 16 in
  [mk_PUSH; mk_POP; mk_NOP]
  @ List.map
      ~f:mk_SWAP
      (List.range ~start:`inclusive 1 ~stop:`inclusive max_idx)
  @ List.map ~f:mk_DUP (List.range ~start:`inclusive 1 ~stop:`inclusive max_idx)
*)

let _ =
  let push = mk_PUSH in
  Printf.printf "mk_PUSH: %s\n" (Z3.Expr.to_string (push.effect 10 1))
