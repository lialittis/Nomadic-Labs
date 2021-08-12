open Protocol

(* read dir *)

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

(* read file functions *)
let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let read_file_in_1_line filename =
  let result = ref "" in
  let ch = open_in filename in
  try
    while true do
      result := !result ^ input_line ch
    done ;
    !result
  with End_of_file -> close_in ch ; !result

let change_file_format filename =
  let str = read_file_in_1_line filename in
  (*let result = "{parameter unit;storage unit ; code " ^ str ^ "}" in*)
  let result = "{" ^ str ^ "}" in
  (*print_endline "change the format of script to:" ;
  print_endline result ;*)
  result

(* Using monad *)
let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error err ->
      (*Lwt.return @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)*)
      Lwt.return @@ Error (Environment.wrap_tztrace err)

let gen_unimportant_context () =
  Context.init 3
  >>=? fun (b, _cs) ->
  Incremental.begin_construction b
  >>=? fun v -> return (Incremental.alpha_ctxt v)
