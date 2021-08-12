open Stack_util
open Z3util
open Protocol
open Michelson_v1_primitives

let code = "{PUSH int 2; PUSH int 3; DROP ; ADD ; NIL operation; PAIR}"

let script = Michelson_interpreteur.parse_string code

let node = Michelson_value_sampler.gen_node code

type node = Michelson_value_sampler.node

(*
let _ =
  let stack0 = sk_init ~length:3 in
  let stack1 = mk_PUSH stack0 1 in
  let stack2 = mk_ADD stack1 in
  let str = Z3.Expr.to_string stack2 in
  print_endline str
*)
module type ENTRIES = sig
  val filter_code : node -> node list

  val stack_evaluation : node list -> Z3.Expr.expr -> int -> int -> Z3.Expr.expr
end

module Mk_Entries () : ENTRIES = struct
  open Micheline

  exception Unprepared_prim

  exception Wrong_type

  exception No_value

  let filter_code node = match node with Seq (_, nl) -> nl | _ -> assert false

  let prim_evaluation prim_node current_sk c j =
    let check_ty_pushed node =
      match node with
      | Prim (_, p, _, _) -> ( match p with T_int -> true | _ -> false )
      | _ -> assert false
    in
    let get_value_pushed node =
      match node with Int (_, value) -> Z.to_int value | _ -> raise Wrong_type
    in
    match prim_node with
    | Prim (_, p, nl, _) -> (
        match p with
        | I_PUSH -> (
            match nl with
            | [] -> assert false
            | hd :: tl ->
                if check_ty_pushed hd then
                  match tl with
                  | [] -> raise No_value
                  | n :: _ ->
                      let value = get_value_pushed n in
                      mk_PUSH current_sk value c j
                else raise Wrong_type )
        | I_ADD -> mk_ADD current_sk c j
        | I_DROP -> mk_DROP current_sk c j
        | I_SWAP -> mk_SWAP current_sk c j
        | _ -> raise Unprepared_prim )
    | _ -> raise Wrong_type

  let rec stack_evaluation node_list current_sk c j =
    match node_list with
    | [] -> current_sk
    | hd :: tl -> (
        match hd with
        | Prim (_, p, _, _) -> (
            match p with
            | I_NIL ->
                current_sk
                (*need more steps to check it occur as last instructions *)
            | _ ->
                let sk' = prim_evaluation hd current_sk c j in
                stack_evaluation tl sk' c (j + 1)
                (* if the index could be added in prim_evaluation ?*) )
        | _ -> raise Wrong_type )
end

(*
let watch node =
  match node with
  | Seq (l, nl) -> (
      print_int l ;
      print_endline "this is a code node" ;
      match nl with
      | [] -> print_endline "strange"
      | hd :: _ -> (
          match hd with
          | Prim (l, p, _, _) ->
              print_int l ;
              print_endline "" ;
              print_endline (string_of_prim p)
          | _ -> print_endline "strange" ) )
  | _ -> print_endline "strange"


let _ = Michelson_value_sampler.to_string node
*)
let _ =
  let module E = Mk_Entries () in
  let initial_sk = sk_init ~length:5 in
  let str = Z3.Expr.to_string initial_sk in
  print_endline str ;
  let program_count = 0 in
  let mk_s =
    func_decl
      ("s_" ^ string_of_int program_count ^ "_" ^ string_of_int 0)
      [stack_sort]
      stack_sort
  in
  let initial_sk = Z3.FuncDecl.apply mk_s [initial_sk] in
  let str = Z3.Expr.to_string initial_sk in
  print_endline str ;
  print_endline "" ;
  let node = node in
  let output_stack =
    E.stack_evaluation (E.filter_code node) initial_sk program_count 1
  in
  let str = Z3.Expr.to_string output_stack in
  print_endline str
