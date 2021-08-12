open Michelson_interpreteur
open Protocol
open Alpha_context
open Utils_sampler
open Mkjson

(* Input parameter parsing *)
let _ =
  if Array.length Sys.argv < 6 then (
    Format.eprintf
      "Excepted: dune exec ./main.exe flag:{sample <number_of_sample> \
       <script_file> <output_file> <to_json_file:yes|no>| optimize \
       <script_file> } \n\
       %!" ;
    exit 1 )
  else if Sys.argv.(1) = "sample" then (
    Format.eprintf "Start sample the inputs/outputs relationships\n%!" ;
    Random.init 12 )
  else if Sys.argv.(1) = "optimize" then (
    Format.eprintf "Please be excited and wait\n%!" ;
    exit 1 )
  else (
    Format.eprintf
      "Excepted: dune exec ./main.exe flag:{sample nsample script_file \
       output_file | optimize script_file}\n\
       %!" ;
    exit 1 )

let nsample = int_of_string Sys.argv.(2)

let filename = Sys.argv.(3)

let output_file = Sys.argv.(4)

let is_to_json = Sys.argv.(5)

let init_context = gen_unimportant_context

let make_context initial_operation_gas =
  init_context () >>=? fun context ->
  (*print_int initial_operation_gas ;*)
  return
    ( Gas_limit_repr.Arith.integral_of_int_exn initial_operation_gas
    |> Gas.set_limit context )

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

let initial_global_gas = Int32.to_int Int32.max_int

let pass_storage_with_para filename initial_global_gas =
  make_context initial_global_gas >>=? fun ctxt ->
  get_para_from_script filename ctxt >>=? fun (ctxt, parameter) ->
  get_storage_from_script filename ctxt >>=? fun (ctxt, storage) ->
  return @@ (ctxt, storage, parameter)

let interprete_script gas_level filename initial_global_gas =
  pass_storage_with_para filename initial_global_gas >>= fun t ->
  match t with
  | Ok (ctxt, storage, parameter) -> (
      (* Run script with a parameter of wrong type *)
      let script = change_file_format filename in
      run_script ctxt script ~storage ~parameter () >>= function
      | Ok t ->
          Format.printf "Interprete script : success\n" ;
          let context = t.ctxt in
          let remaining = gas_level context in
          let sto =
            print (Michelson_v1_primitives.strings_of_prims t.storage)
          in
          return @@ ((storage, parameter), sto, remaining)
      | Error _ -> Lwt.fail_with "Unexcepted Error in run_script" )
  | Error _ -> Lwt.fail_with "Unexcepted Error in pass_storage"

(* [TEST] test default *)
(*
let default_file = "simple_contract.tz"

let output = interprete_script default_file
*)
(* [ENDTEST]*)
(*
let save filename terms =
  Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename terms
*)

let open_channel output_filename =
  Lwt_io.open_file
    ~flags:[Unix.O_APPEND; Unix.O_CREAT; Unix.O_RDWR]
    ~mode:Lwt_io.Output
    output_filename

let open_channel_json output_filename =
  Lwt_io.open_file
    ~flags:[Unix.O_TRUNC; Unix.O_CREAT; Unix.O_RDWR]
    ~mode:Lwt_io.Output
    output_filename

let save_output oc terms = Lwt_io.fprintl oc terms

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

let f filename output_filename nsample initial_global_gas =
  let output_file = Filename.concat "output" output_filename in
  open_channel output_file >>= fun oc ->
  let rec loop i =
    print_endline ("****** " ^ string_of_int (10 - i) ^ " run ******") ;
    interprete_script operation_gas_level filename initial_global_gas
    >>= function
    | Ok ((para, sto_bef), sto_aft, remaining) ->
        let consumed_gas = gas_to_string remaining initial_global_gas in
        to_json para sto_bef sto_aft ;
        save_output oc (para ^ ";" ^ sto_bef ^ "=>" ^ sto_aft ^ consumed_gas)
        >>= fun _ -> if i <> 1 then loop (i - 1) else Lwt.return_unit
    | Error _ -> Lwt.fail_with "Unexcepted Errors!"
  in
  loop nsample >>= fun () -> Lwt_io.close oc

let save_to_json filename output_filename nsample initial_global_gas =
  let output_file = Filename.concat "output" output_filename in
  open_channel_json output_file >>= fun oc ->
  save_output oc "{ \"samples\": {" >>= fun _ ->
  (* a bad way to construct the start of the json *)
  let rec loop i =
    print_endline ("****** " ^ string_of_int (nsample - i) ^ " run ******") ;
    interprete_script operation_gas_level filename initial_global_gas
    >>= function
    | Ok ((para, sto_bef), sto_aft, _) ->
        let relation_t =
          { index = string_of_int (nsample - i);
            relation =
              { input = { parameter = para; storage_i = sto_bef };
                output = { storage_o = sto_aft }
              }
          }
        in
        save_output
          oc
          ( "\""
          ^ string_of_int (nsample - i)
          ^ "\" : "
          ^ to_full_relation_json relation_t
          ^ "," )
        >>= fun _ -> if i <> 1 then loop (i - 1) else Lwt.return_unit
    | Error _ -> Lwt.fail_with "Unexcepted Errors!"
  in
  loop nsample >>= fun () ->
  save_output oc "}" >>= fun _ -> Lwt_io.close oc

let () =
  if is_to_json == "no" then
    Lwt_main.run @@ f filename output_file nsample initial_global_gas
  else
    Lwt_main.run @@ save_to_json filename output_file nsample initial_global_gas

(*
(* old version *)
let () =
  let oc = open_channel output_file in
  let rec loop i =
    interprete_script operation_gas_level filename
    >>= function
    | Ok ((para, sto_bef), sto_aft, remaining) ->
        if i = 0 then
          save_output oc (para ^ ";" ^ sto_bef ^ ";" ^ sto_aft)
          >>= fun _ -> Lwt_io.close oc
        else
          save_output oc (para ^ ";" ^ sto_bef ^ ";" ^ sto_aft)
          >>= fun _ -> loop (i - 1)
    | Error _ ->
        Lwt.fail_with "Unexcepted Error!" >>= fun _ -> Lwt_io.close oc
  in
  Lwt_main.run (loop nsample)
*)

(* apply sampler result to the interpreter *)
(*
let apply global_type_string =
  print_prims global_type_string
  >>=? fun node ->
  let parameter = node in
  test_interpreter parameter
  >>= fun res ->
  match res with
  | Ok exec_res ->
      let storage = exec_res.storage in
      let str : string =
        Michelson_interpreteur.print (strings_of_prims storage)
      in
      Format.printf "The storage of execution_result is :" ;
      print_string (str ^ "\n") ;
      return @@ storage
  | Error errs ->
      Format.printf "something wrong!\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let res = apply global_type_string
*)
