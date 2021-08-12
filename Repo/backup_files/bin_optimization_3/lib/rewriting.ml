open Michelson_interpreteur
open Protocol
open Michelson_value_sampler

let _ =
  let contract =
    "{parameter unit; storage unit; code {CAR ; NIL operation; PAIR }}"
  in
  let contract_expr = get_ast contract in
  let contract_node = gen_node contract_expr in
  let str = to_string contract_node in
  Printf.printf "The contract node is like : %s\n" str
