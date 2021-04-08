(** Testing
    -------
    Component:    
    Invocation:
    Dependencies:
    Subject:
*)

let print expr : string =
  expr
  |> Micheline_printer.printable (fun s -> s)
  |> Format.asprintf "%a" Micheline_printer.print_expr

(*
let expands 
    (original : (Micheline_parser.location, string) Micheline.node)
    (expanded : (Micheline_parser.location, string) Micheline.node) = 
*)

let expand original = 
  let source = print (Micheline.strip_locations original) in
  Michelson_v1_parser.expand_all ~source ~original

(*let results = expand_all ~source ~original*)

let zero_loc = Micheline_parser.location_zero

let results prim_name=
  expand (Prim (zero_loc, prim_name, [], []))

let output =
  results "CMPNEQ"
;;

output;;

let parser_result str = 
  Michelson_v1_parser.parse_expression str;;

let output2 = parser_result "CMPNEQ"
;;

output2;;

print output2.expanded;;

print_endline "ending";;
