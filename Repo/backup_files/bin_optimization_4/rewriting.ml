(*open Michelson_interpreteur
open Protocol*)
open Michelson_value_sampler
open Printf
open Protocol
open Michelson_v1_primitives

type node = Michelson_value_sampler.node

exception Exp

let print_node node =
  (* the contract is parsed into node *)
  let str = to_string node in
  Printf.printf "The node is : %s\n" str

(* possibility of stoping exploration*)
let p_of_end = 0.4

let rec explore (t : node) =
  match t with
  | Int (_, _) | String (_, _) | Bytes (_, _) -> t
  | Prim (_, _, node_list, _) ->
      Random.self_init () ;
      let actual_p = Random.float 1. in
      if actual_p >= p_of_end then
        match node_list with
        | [] -> t
        | child -> (
            let len = List.length node_list in
            Random.self_init () ;
            let p = Random.int len in
            match List.nth child p with
            | None -> raise Exp
            | Some n -> explore n )
      else t
  | Seq (_, node_list) ->
      Random.self_init () ;
      let actual_p = Random.float 1. in
      if actual_p >= p_of_end then
        match node_list with
        | [] -> t
        | child -> (
            let len = List.length node_list in
            Random.self_init () ;
            let p = Random.int len in
            match List.nth child p with
            | None -> raise Exp
            | Some n -> explore n )
      else t

module type Rules = sig
  type t = node

  val check_types_2 : t -> bool

  val swap_1 : t -> int list

  val cut : t -> int -> int list -> t
end

let mk_rules () =
  ( module struct
    type t = node

    let check_types_2 (t : t) =
      match t with
      | Int _ | String _ | Bytes _ -> true
      | Prim _ -> false
      | Seq _ -> false

    exception Wrong_type

    let swap_1 (t : t) =
      match t with
      | Int _ | String _ | Bytes _ -> raise Wrong_type
      | Prim (_, _, _, _) as node ->
          (* here the print is the to_string function *)
          if "SWAP" = print (strings_of_prims (Micheline.strip_locations node))
          then (
            print_endline "possible" ;
            [] )
          else (
            print_endline "pass" ;
            [] (* there is possible *) )
      | Seq (_, node_list) -> (
          match node_list with
          | [] ->
              print_endline "impossible" ;
              []
          | _ ->
              let len = List.length node_list in
              let rec loop index ready length_of_list locations_tocut =
                if index = length_of_list then (
                  print_endline "not find" ;
                  locations_tocut )
                else
                  match List.nth node_list index with
                  | None -> raise Exp
                  | Some node ->
                      let str =
                        print
                          (strings_of_prims (Micheline.strip_locations node))
                      in
                      if "SWAP" = str && ready = true then (
                        print_endline "find one" ;
                        let l = Micheline.location node in
                        l :: locations_tocut )
                      else if "SWAP" = str then
                        loop
                          (index + 1)
                          true
                          length_of_list
                          (Micheline.location node :: locations_tocut)
                      else loop (index + 1) false length_of_list locations_tocut
              in
              loop 0 false len [] )

    let rec cut (t : t) (location_sn : int) (location_cn_list : int list) =
      match t with
      | Int _ | String _ | Bytes _ -> t
      | Prim (l, p, nl, a) -> (
          match nl with
          | [] -> t
          | list ->
              let new_list =
                List.map
                  (fun node -> cut node location_sn location_cn_list)
                  list
              in
              Micheline.Prim (l, p, new_list, a) )
      | Seq (l, node_list) -> (
          if l = location_sn then
            let rec aux node_list location_cn_list =
              match location_cn_list with
              | [] -> node_list
              | h :: t ->
                  let new_nl =
                    List.filter
                      (fun node -> Micheline.location node <> h)
                      node_list
                  in
                  aux new_nl t
            in
            let new_list = aux node_list location_cn_list in
            Micheline.Seq (l, new_list)
          else
            match node_list with
            | [] -> t
            | list ->
                let new_list =
                  List.map
                    (fun node -> cut node location_sn location_cn_list)
                    list
                in
                Micheline.Seq (l, new_list) )

    (*should I check all children or only the next level ?*)
  end : Rules )

let rewriting_rules = mk_rules ()

let rewrite (root : node) (node : node) (module R : Rules) =
  let location = Micheline.location node in
  let locations_tocut = R.swap_1 node in
  let sol = R.cut root location locations_tocut in
  sol

(* A stupid test function, from which we can tell the structure of the tree *)
let _ =
  let contract = "{code {CAR ; SWAP ; SWAP ; NIL operation; PAIR }}" in
  (*let contract_expr = get_ast contract in*)
  let contract_node = gen_node contract in

  (* To read the contract *)
  (*
  let rec foo
      (node : node)
        (* ( location, Tezos_protocol_alpha.Protocol.Michelson_v1_primitives.prim ) Micheline.node *)
      =
    match node with
    | Int (l, _) -> printf "Int location for the first node : %d\n" l
    | String (l, _) -> printf "String location for the first node : %d\n" l
    | Bytes (l, _) -> printf "Bytes location for the first node : %d\n" l
    | Seq (l, nl) ->
        printf "Seq location for the first node : %d\n" l ;
        let str_list = List.map (fun node -> to_string node) nl in
        for i = 0 to List.length str_list do
          match List.nth str_list i with
          | None -> print_endline ""
          | Some str -> print_endline str
        done ;
        let rec loop index =
          if index = List.length nl then print_endline ""
          else
            let x = List.nth nl index in
            match x with
            | None -> raise Exp
            | Some t ->
                foo t ;
                loop (index + 1)
        in
        loop 0
    | Prim (l, _, nl, a) ->
        printf "Prim location for the first node : %d\n" l ;
        let str_list = List.map (fun node -> to_string node) nl in
        for i = 0 to List.length str_list do
          match List.nth str_list i with
          | None -> print_endline ""
          | Some str -> print_endline str
        done ;
        for i = 0 to List.length a do
          match List.nth a i with
          | None -> print_endline "no annot"
          | Some str -> printf "annot is :%s\n" str
        done ;
        let rec loop index =
          if index = List.length nl then print_endline ""
          else
            let x = List.nth nl index in
            match x with
            | None -> raise Exp
            | Some t ->
                foo t ;
                loop (index + 1)
        in
        loop 0
  in
  foo contract_node ;
*)
  let stop_node = explore contract_node in
  Random.self_init () ;
  print_node stop_node ;
  let location = Micheline.location stop_node in
  printf "location of stop node is %d\n" location ;
  let new_t = rewrite contract_node stop_node rewriting_rules in
  print_node new_t
