open Protocol
open Alpha_context
open Yojson.Basic.Util
open Utils_sampler

(* move json reading (from relationships file) and samples generation here *)
(* including storage and parameter generations*)
type sample = { vars : string * string; res : string }

let operation_gas_level context =
  let open Gas in
  match level context with
  | Limited { remaining } -> remaining
  | _ ->
      (* because this function is called after [set_gas_limit]. *)
      assert false

let get_para_from_script filename ctxt =
  Parse_parameters_storage.get_para filename >>=? fun parameter ->
  return @@ (ctxt, parameter)

let get_storage_from_script filename ctxt =
  Parse_parameters_storage.get_storage filename >>=? fun storage ->
  return @@ (ctxt, storage)

(* random values : need to reorganize [TODO] *)
let pass_storage_with_para filename initial_global_gas =
  make_context initial_global_gas >>=? fun ctxt ->
  get_para_from_script filename ctxt >>=? fun (ctxt, parameter) ->
  get_storage_from_script filename ctxt >>=? fun (ctxt, storage) ->
  return @@ (ctxt, storage, parameter)

let init_context = gen_unimportant_context

(* open channel by Lwt_io *)
let open_channel output_filename =
  Lwt_io.open_file
    ~flags:[Unix.O_APPEND; Unix.O_CREAT; Unix.O_RDWR]
    ~mode:Lwt_io.Output
    output_filename

(* open channel for json file by Lwt_io *)
let open_channel_json output_filename =
  Lwt_io.open_file
    ~flags:[Unix.O_TRUNC; Unix.O_CREAT; Unix.O_RDWR]
    ~mode:Lwt_io.Output
    output_filename

(* save terms for open channel *)
let save_output oc terms = Lwt_io.fprintl oc terms

(* convert gas to string *)
let gas_to_string remaining initial_global_gas =
  Gas.pp_cost Format.str_formatter remaining ;
  let remaining_gas = float_of_string (Format.flush_str_formatter ()) in
  (*print_endline ("remaining_gas" ^ string_of_float remaining_gas) ;*)
  let str =
    "#consumed gas :"
    ^ string_of_float
        ( float_of_int initial_global_gas
        -. (remaining_gas (* in milligas. *) *. 0.001) )
  in
  print_endline str ;
  str

(* read relationships file in json *)
let get_sample_input json =
  let parameter = to_string (member "parameter" json) in
  let sto = to_string (member "storage_i" json) in
  let var = (parameter, sto) in
  var

let get_sample_output json =
  let sto = member "storage_o" json in
  let var = to_string sto in
  var

(* get sample *)
let get_sample json =
  { vars = get_sample_input (member "input" json);
    res = get_sample_output (member "output" json)
  }

let get_samples json =
  json |> member "samples" |> to_assoc
  |> List.map (fun (_, t) -> get_sample (member "relation" t))
  |> Array.of_list

(* get relationships *)
let of_outputs (samples : sample array) =
  let len = Array.length samples in
  let rec aux arr i =
    if i < len then aux (samples.(i).res :: arr) (i + 1) else arr
  in
  Array.of_list (aux [] 0)
