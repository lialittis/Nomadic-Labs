open Protocol
open Alpha_context

type 'a ty = 'a Script_typed_ir.ty

type ex_ty = Script_ir_translator.ex_ty

type ex_value = Ex_Value : 'a ty * 'a -> ex_value

val get_ast : string -> Script.expr

val parse_expression : ?check:bool -> string -> Micheline_parser.node

type location = int

type node = (location, Script.prim) Micheline.node

val gen : 'a ty -> 'a

val gen_exvalue : ex_ty -> ex_value

val gen_node :
  string -> (location, Michelson_v1_primitives.prim) Micheline.node

val print : string Micheline.canonical -> string

val print_prims :
  string ->
  (string, Environment.Error_monad.error Environment.Error_monad.trace) result
  Lwt.t
