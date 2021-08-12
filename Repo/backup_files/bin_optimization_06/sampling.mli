open Protocol
open Alpha_context

type sample = { vars : string * string; res : string }

val operation_gas_level : context -> Gas.Arith.fp

val get_para_from_script :
  string -> 'a -> ('a * string, error Error_monad.trace) result Lwt.t

val get_storage_from_script :
  string -> 'a -> ('a * string, error Error_monad.trace) result Lwt.t

val init_context : unit -> (context, tztrace) result Lwt.t

val open_channel : string -> Lwt_io.output Lwt_io.channel Lwt.t

val open_channel_json : string -> Lwt_io.output Lwt_io.channel Lwt.t

val save_output : Lwt_io.output_channel -> string -> unit Lwt.t

val gas_to_string : Gas.cost -> int -> string

val get_sample_input : Yojson.Basic.t -> string * string

val get_sample_output : Yojson.Basic.t -> string

val get_sample : Yojson.Basic.t -> sample

val get_samples : Yojson.Basic.t -> sample array

val pass_storage_with_para :
  string -> int -> (context * string * string, error trace) result Lwt.t

val of_outputs : sample array -> string array
