open Protocol
open Alpha_context

(* read dir *)
val dir_is_empty : string -> bool

val dir_contents : string -> string list

(* read file *)
val read_file : string -> string

val read_file_in_1_line : string -> string

(* change format *)
val change_file_format : string -> string

val ( >>=?? ) :
  ('a, Environment.Error_monad.error Environment.Error_monad.trace) result
  Lwt.t ->
  ('a -> ('b, error trace) result Lwt.t) ->
  ('b, error trace) result Lwt.t

val gen_unimportant_context : unit -> (t, error trace) result Lwt.t
