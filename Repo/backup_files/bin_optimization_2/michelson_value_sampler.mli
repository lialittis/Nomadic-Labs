open Protocol
open Alpha_context

type 'a ty = 'a Script_typed_ir.ty

type ex_ty = Script_ir_translator.ex_ty

type ex_value = Ex_Value : 'a ty * 'a -> ex_value

type global_type_string = string

val get_ast : global_type_string -> Script.expr

val parse_expression :
  ?check:bool -> global_type_string -> Micheline_parser.node

type location = int

type node = (location, Script.prim) Micheline.node

val gen_node :
  global_type_string -> (location, Michelson_v1_primitives.prim) Micheline.node

val print_prims :
  global_type_string ->
  ( global_type_string,
    Environment.Error_monad.error Environment.Error_monad.trace )
  result
  Lwt.t
