(*open Michelson_interpreteur
open Protocol*)
open Michelson_value_sampler
open Printf
open Protocol
open Michelson_v1_primitives
open Heuristic_utils.Heuristics
open Tree
open Michelson_interpreteur
open Sampling
open Utils_sampler

type node = Michelson_value_sampler.node

exception Exp

let print_node node =
  (* the contract is parsed into node *)
  let str = to_string node in
  Printf.printf "The node iss : %s\n" str

let get_script = Parse_parameters_storage.script_string_from_file

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

let mk_rules (module D : Dist) =
  ( module struct
    module M = Mk_Mutator ()

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

    (*should I check all children or only the next level ? For basic block, no needs *)

    let gen_init_node (filename : string) =
      let script = get_script filename in
      let node = Michelson_value_sampler.gen_node script in
      let prim_para = get_prim_type node "parameter" in
      let prim_storage = get_prim_type node "storage" in
      let initial_node = M.init_prim prim_para prim_storage in
      let node =
        M.mutate_2 initial_node 100 ~additions:1 ~max_loop:10 ~max_mutation:200
      in
      node

    let interprete_random_node (samples : sample array) (node : t) ctxt
        (initial_global_gas : int) =
      let new_script = to_string node in
      let nsample = Array.length samples in
      let rec loop i pair =
        let arr = pair in
        let (parameter, storage) = samples.(i).vars in
        interprete_script operation_gas_level new_script ctxt storage parameter
        >>= function
        | Ok (_, s', remaining) ->
            let consumed_gas = gas_to_string remaining initial_global_gas in
            if i <> 0 then loop (i - 1) (s' :: arr)
            else Lwt.return @@ (Array.of_list (s' :: arr), consumed_gas)
        (*save_output oc (para ^ ";" ^ sto_bef ^ "=>" ^ sto_aft ^ consumed_gas)*)
        | Error _ -> Lwt.fail_with "Unexcepted Errors!"
      in
      loop (nsample - 1) []

    type sol = { node : t; cost : string; distance : float }

    let distance_list = ref []

    let process (filename : string) (relationships_file : string)
        initial_global_gas =
      let json = Yojson.Basic.from_file relationships_file in
      let samples = get_samples json in
      (*let nsample = Array.length samples in*)
      let outputs = of_outputs samples in

      let max_loop = 20 in

      let gen_sol node outputs outputs' gas =
        let d = sum_dists D.dist outputs outputs' in
        Printf.printf "for the node %s , distance is %f\n" (to_string node) d ;
        return @@ { node; cost = gas; distance = d }
      in

      (* jump/update from the beginning node for iterated local search *)
      let perturbate outputs ctxt =
        Random.self_init () ;
        gen_init_node filename >>=? fun updated_node ->
        print_node updated_node ;
        interprete_random_node samples updated_node ctxt initial_global_gas
        >>= fun (outputs', gas) -> gen_sol updated_node outputs outputs' gas
      in

      (* loop for local search *)
      let rec loop sol n ctxt =
        if n = max_loop || D.is_zero sol.distance then
          let _ = distance_list := sol.distance :: !distance_list in
          return @@ sol
        else
          M.mutate_2 sol.node 200 ~additions:1 ~max_loop:10 ~max_mutation:200
          >>=? fun mutated ->
          interprete_random_node samples mutated ctxt initial_global_gas
          >>= fun (outputs', gas) ->
          gen_sol mutated outputs outputs' gas >>=? fun sol' ->
          if sol'.distance < sol.distance then loop sol' 0 ctxt
          else loop sol (n + 1) ctxt
      in

      (* find the best sol in a sol list *)
      let best_sol_list sol_list =
        let rec aux sol_list best =
          match sol_list with
          | [] -> best
          | hd :: tl ->
              let newbest = if hd.distance < best.distance then hd else best in
              aux tl newbest
        in
        match sol_list with [] -> assert false | hd :: tl -> aux tl hd
      in

      (* lookahead the n best sol (best means better than sol at least) by mutating/local search and collect them into a list *)
      let lookahead sol nbest max ctxt =
        let rec aux sol n max =
          match (n, max) with
          | (0, _) | (_, 0) -> return @@ []
          | (_, _) ->
              loop sol 0 ctxt >>=? fun sol' ->
              if sol'.distance < sol.distance then
                aux sol (n - 1) (max - 1) >>=? fun remain ->
                return @@ (sol' :: remain)
                (*match remain with
                  | [] -> Lwt.return [sol']
                  | _ -> Lwt.return (sol' :: remain)*)
              else
                aux sol n (max - 1) >>=? fun remain -> return @@ (sol' :: remain)
        in
        aux sol nbest max
      in

      let bestlookahead = 5 in
      let maxlookahead = 10 in

      make_context initial_global_gas >>= fun t ->
      match t with
      | Ok ctxt ->
          (* start the process*)
          Random.self_init () ;
          (* initial node [TODO] could be optimized *)
          perturbate outputs ctxt >>=? fun init_sol ->
          let best_sol = init_sol in
          let max_iter = 1000 in
          (* using candidates to store every local optimum solution *)
          let rec aux candidates best_sol ctxt n =
            Random.self_init () ;

            (* [TODO] verify this*)
            let d = best_sol.distance in
            if D.is_zero d || n > max_iter then return @@ candidates
            else
              perturbate outputs ctxt >>=? fun perturbated_sol ->
              lookahead perturbated_sol bestlookahead maxlookahead ctxt
              >>=? fun sol_list ->
              let maybe_better_sol = best_sol_list sol_list in
              aux
                (maybe_better_sol :: candidates)
                ( if Compare.Float.(maybe_better_sol.distance < d) then
                  maybe_better_sol
                else best_sol )
                ctxt
                (n + 1)
          in
          aux [] best_sol ctxt 0
      | Error _ -> Lwt.fail_with "Wrong ctxt!"

    (*
    let rec process (candidates:t list) (para:prim) (sto:prim) =
      let init_node = M.init_prim para sto in
      let dist = D.dist
*)
  end : RULES )

(*
let dist = mk_hamming ()

let rewriting_rules = mk_rules dist
*)

let rewrite_random contract_file relationships_file initial_global_gas
    (module R : RULES) =
  Random.self_init () ;
  R.process contract_file relationships_file initial_global_gas >>= function
  | Ok l ->
      let rec aux (l : R.sol list) =
        match l with
        | [] -> Lwt.return @@ ()
        | hd :: tl ->
            print_node hd.node ;
            aux tl
      in
      aux l
  | Error _ -> assert false

(* *)
let rewrite_det (root : node) (node : node) (module R : RULES) =
  let location = Micheline.location node in
  let locations_tocut = R.swap_1 node in
  let sol = R.cut root location locations_tocut in
  sol

(* A stupid test function, from which we can tell the structure of the tree *)
(*
let contract = "{code {CAR ; SWAP ; SWAP ; NIL operation; PAIR }}" in
  (*let contract_expr = get_ast contract in*)
*)
let simplify contract (module R : RULES) =
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
  let new_t = rewrite_det contract_node stop_node (module R) in
  print_node new_t ;
  to_string new_t
