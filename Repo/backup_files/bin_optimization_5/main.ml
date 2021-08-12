open Michelson_interpreteur
open Utils_sampler
open Mkjson
open Rewriting_rules
open Heuristic_utils
open Heuristics
open Sampling

type args_t =
  { mutable option : string;
    mutable nsamples : int;
    mutable script_file : string;
    mutable relationships_file : string;
    mutable output_file : string;
    mutable to_json_file : bool ref
  }

let parse_args () =
  let usage_msg =
    "./../../_build/default/src/bin_optimization/main.exe <-option> <filename> \
     [<-n>] [<-r>] [<-o>] [<-json>]"
  in
  let args =
    { option = "";
      nsamples = 10;
      script_file = "";
      relationships_file = "";
      output_file = "";
      to_json_file = ref false
    }
  in
  let open Arg in
  parse
    [ ( "-option",
        String (fun t -> args.option <- t),
        "[sample | optimize] : option to generate I/O relations or optimize \
         one Michelson Program" );
      ( "-n",
        Int (fun n -> args.nsamples <- n),
        "number of samples to nenerated (sample), default value is 10" );
      ( "-r",
        String (fun t -> args.relationships_file <- t),
        "the json file contaioning I/O relationships (optimize)" );
      ( "-o",
        String (fun t -> args.output_file <- t),
        "output file for relationship (sample)" );
      ( "-json",
        Set args.to_json_file,
        "if the output file in json format, default value is false" ) ]
    (fun t -> args.script_file <- t)
    usage_msg ;
  args

(*
let init_context = gen_unimportant_context

let make_context = make_context

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

let initial_global_gas = initial_global_gas

let pass_storage_with_para filename initial_global_gas =
  make_context initial_global_gas >>=? fun ctxt ->
  get_para_from_script filename ctxt >>=? fun (ctxt, parameter) ->
  get_storage_from_script filename ctxt >>=? fun (ctxt, storage) ->
  return @@ (ctxt, storage, parameter)

*)

(*
let interprete_script gas_level filename initial_global_gas =
  pass_storage_with_para filename initial_global_gas >>= fun t ->
  match t with
  | Ok (ctxt, storage, parameter) -> (
      (* Run script with a parameter of wrong type *)
      let script = change_file_format filename in
      let new_script = rewrite script in
      run_script ctxt script ~storage ~parameter () >>= function
      | Ok t -> (
          Format.printf "Interprete script : success\n" ;
          let context = t.ctxt in
          let remaining = gas_level context in
          let sto =
            print (Michelson_v1_primitives.strings_of_prims t.storage)
          in
          run_script ctxt new_script ~storage ~parameter () >>= function
          | Ok t' ->
              let context' = t'.ctxt in
              let remaining' = gas_level context' in
              let sto' =
                print (Michelson_v1_primitives.strings_of_prims t'.storage)
              in
              return
              @@ ((storage, parameter), (sto, remaining), (sto', remaining'))
          | Error _ -> Lwt.fail_with "Unexcepted Error in run_script" )
      | Error _ -> Lwt.fail_with "Unexcepted Error in run_script" )
  | Error _ -> Lwt.fail_with "Unexcepted Error in pass_storage"

*)

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

(*
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
*)

let f filename output_filename nsample initial_global_gas =
  let output_file = Filename.concat "output" output_filename in
  open_channel output_file >>= fun oc ->
  let script = change_file_format filename in
  let rec loop i =
    print_endline ("****** " ^ string_of_int (10 - i) ^ " run ******") ;
    pass_storage_with_para filename initial_global_gas >>= fun t ->
    match t with
    | Ok (ctxt, storage, parameter) -> (
        interprete_script operation_gas_level script ctxt storage parameter
        >>= function
        | Ok ((para, sto_bef), sto_aft, remaining) ->
            let consumed_gas = gas_to_string remaining initial_global_gas in
            (*to_json para sto_bef sto_aft ;*)
            save_output oc (para ^ ";" ^ sto_bef ^ "=>" ^ sto_aft ^ consumed_gas)
            >>= fun _ -> if i <> 1 then loop (i - 1) else Lwt.return_unit
        | Error _ -> Lwt.fail_with "Unexcepted Errors!" )
    | Error _ -> Lwt.fail_with "Unexcepted Error in pass_storage"
  in
  loop nsample >>= fun () -> Lwt_io.close oc

let save_to_json filename output_filename nsample initial_global_gas =
  let output_file = Filename.concat "output" output_filename in
  open_channel_json output_file >>= fun oc ->
  save_output oc "{ \"samples\": {" >>= fun _ ->
  (* a bad way to construct the start of the json *)
  let rec loop i =
    print_endline ("****** " ^ string_of_int (nsample - i) ^ " run ******") ;
    let script = change_file_format filename in
    pass_storage_with_para filename initial_global_gas >>= fun t ->
    match t with
    | Ok (ctxt, storage, parameter) -> (
        interprete_script operation_gas_level script ctxt storage parameter
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
            let save_string =
              if i = 1 then
                "\""
                ^ string_of_int (nsample - i)
                ^ "\" : "
                ^ to_full_relation_json relation_t
              else
                "\""
                ^ string_of_int (nsample - i)
                ^ "\" : "
                ^ to_full_relation_json relation_t
                ^ ","
            in
            save_output oc save_string >>= fun _ ->
            if i <> 1 then loop (i - 1) else Lwt.return_unit
        | Error _ -> Lwt.fail_with "Unexcepted Errors!" )
    | Error _ -> Lwt.fail_with "Unexcepted Error in pass_storage"
  in
  loop nsample >>= fun () ->
  save_output oc "} }" >>= fun _ -> Lwt_io.close oc

(*
type sample = { vars : string * string; res : string }

let get_sample_input json =
  let parameter = to_string (member "parameter" json) in
  let sto = to_string (member "storage_i" json) in
  let var = (parameter, sto) in
  var

let get_sample_output json =
  let sto = member "storage_o" json in
  let var = to_string sto in
  var

let get_sample json =
  { vars = get_sample_input (member "input" json);
    res = get_sample_output (member "output" json)
  }

let get_samples json =
  json |> member "samples" |> to_assoc
  |> List.map (fun (_, t) -> get_sample (member "relation" t))
  |> Array.of_list
*)

(* test function *)
(*
let f' (module D : Dist) filename relationships_file initial_global_gas =
  let json = Yojson.Basic.from_file relationships_file in
  let script = change_file_format filename in
  let rules = mk_rules (module D) in
  let new_script = simplify script rules in
  let samples = get_samples json in
  let nsample = Array.length samples in
  Printf.printf "nsample is %d\n" nsample ;
  let rec loop i d =
    print_endline ("****** " ^ string_of_int (nsample - i) ^ " run ******") ;
    make_context initial_global_gas >>= fun t ->
    match t with
    | Ok ctxt -> (
        let (parameter, storage) = samples.(i).vars in
        let sto_aft = samples.(i).res in
        interprete_script operation_gas_level new_script ctxt storage parameter
        >>= function
        | Ok ((_, _), sto_aft', remaining) ->
            let _ = gas_to_string remaining initial_global_gas in
            (*to_json para sto_bef sto_aft ;*)
            let distance = D.dist sto_aft sto_aft' +. d in
            if i <> 0 then loop (i - 1) distance else Lwt.return_unit
        | Error _ -> Lwt.fail_with "Unexcepted Errors!" )
    | Error _ -> Lwt.fail_with "Unexcepted Errors!"
  in
  loop (nsample - 1) 0.
*)

(* Input parameter parsing *)
let () =
  let args = parse_args () in
  let option = args.option in
  let nsample = args.nsamples in
  let filename = args.script_file in
  let output_file = args.output_file in
  let is_to_json = !(args.to_json_file) in
  let relationships_file = args.relationships_file in

  if option = "sample" then
    if filename = "" then print_endline "missed input filename"
    else if output_file = "" then print_endline "missed option -o"
    else if not is_to_json then
      Lwt_main.run @@ f filename output_file nsample initial_global_gas
    else
      Lwt_main.run
      @@ save_to_json filename output_file nsample initial_global_gas
  else if option = "optimize" then
    if filename = "" then print_endline "missed input filename"
    else if relationships_file = "" then print_endline "missed option -r"
    else
      let dist = mk_arith () in
      let rules = mk_rules dist in
      (*Lwt_main.run @@ f' dist filename relationships_file initial_global_gas*)
      Lwt_main.run
      @@ rewrite_random filename relationships_file initial_global_gas rules
  else Printf.printf "wrong option!\n"
