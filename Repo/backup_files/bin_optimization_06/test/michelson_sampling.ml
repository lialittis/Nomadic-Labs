(*open Protocol*)

(*let filename = "bef/sample"*)

(*let filename_save = "aft/sample"*)

(*tools*)

(** [dir_is_empty dir] is true, if [dir] contains no files except
 * "." and ".."
 *)
let dir_is_empty dir = Array.length (Sys.readdir dir) = 0

(** [dir_contents] returns the paths of all regular files that are
 * contained in [dir]. Each file is a path starting with [dir].
  *)
let dir_contents dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs ->
        loop (f :: result) fs
    | [] ->
        result
  in
  loop [] [dir]

let save filename terms =
  Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename terms

let add_para_stor filename =
  let add_contents = "parameter unit ; storage unit" in
  let oc = open_out filename in
  Printf.fprintf oc "%s\n" add_contents ;
  close_out oc

let foo filename =
  let micheline_data_list = Michelson_generation.load_file filename in
  let result =
    List.map
      (fun micheline_data ->
        match micheline_data with
        | Michelson_generation.Code {term = t; bef = _} ->
            (*Test_helpers.typecheck_by_tezos
            micheline_data.bef
            micheline_data.term ;*)
            let printable =
              Micheline_printer.printable
                Tezos_protocol_alpha.Protocol.Michelson_v1_primitives
                .string_of_prim
                t
            in
            (*Format.printf "result :" ;*)
            let terms =
              Format.asprintf "%a" Micheline_printer.print_expr printable
            in
            let filename_no_prefix =
              match List.nth (String.split_on_char '/' filename) 1 with
              | None ->
                  ""
              | Some string ->
                  string
            in
            let filename_save = "aft/" ^ filename_no_prefix ^ ".tz" in
            save filename_save terms ;
            (*add_para_stor filename_save ;*)
            Format.eprintf "%a@." Micheline_printer.print_expr printable
        | Michelson_generation.Data {term = t; typ = _} ->
            let printable =
              Micheline_printer.printable
                Tezos_protocol_alpha.Protocol.Michelson_v1_primitives
                .string_of_prim
                t
            in
            (*Format.printf "result :" ;*)
            let terms =
              Format.asprintf "%a" Micheline_printer.print_expr printable
            in
            let filename_no_prefix =
              match List.nth (String.split_on_char '/' filename) 1 with
              | None ->
                  ""
              | Some string ->
                  string
            in
            let filename_save = "aft/" ^ filename_no_prefix ^ ".tz" in
            save filename_save terms ;
            (*add_para_stor filename_save ;*)
            Format.eprintf "%a@." Micheline_printer.print_expr printable)
      (*| _ ->
            Format.printf "wrong samples"*)
      micheline_data_list
  in
  match result with [] -> () | _ -> Format.printf "success!"

(*let results = List.map (fun filename -> foo filename) dir_contents*)

;;
List.iter foo (dir_contents "bef")

(*
let rec do_all foo dir_contents =
  match dir_contents with
  | [] ->
      ()
  | head :: tail ->
      foo head ; do_all foo tail

;;
do_all foo dir_contents
*)
