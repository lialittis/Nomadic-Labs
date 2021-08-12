open Protocol
open Alpha_context
open Michelson_v1_primitives
open Script_interpreter

val dir_is_empty : string -> bool

val dir_contents : string -> string trace

val read_file : string -> string

val test_context : unit -> (context, tztrace) result Lwt.t

val run_script :
  context ->
  ?step_constants:step_constants ->
  string ->
  ?entrypoint:string ->
  storage:string ->
  parameter:string ->
  unit ->
  (execution_result, error trace) result Lwt.t

val print : string Micheline.canonical -> string

val print_expanded : prim Micheline.canonical -> unit

val interprete_script : string -> (execution_result, tztrace) result Lwt.t
