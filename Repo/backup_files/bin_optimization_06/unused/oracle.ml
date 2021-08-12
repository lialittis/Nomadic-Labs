open Yojson.Basic.Util

type variable = { name : string; sz : int }

type constant = { name : string; value : Bitvector.t }

type sample =
  { vars : (variable * Bitvector.t) array; res : variable * Bitvector.t }

type t =
  { ctx : variable array * variable;
    consts : constant array;
    samples : sample array;
    ops : string list option
  }

module type ORACLE = sig
  val nvars : unit -> int

  val nconsts : unit -> int

  val nsamples : unit -> int

  val var_values : variable -> Bitvector.t array

  val const_values : constant -> Bitvector.t array

  val out_values : unit -> Bitvector.t array

  val out_var : unit -> variable

  val random_var : unit -> variable

  val random_const : unit -> constant

  val print : unit -> unit

  val vars : unit -> variable array

  val consts : unit -> constant array

  val ops : unit -> string list option

  val const_of_int : int -> constant

  val const_of_bitv32 : Bitvector.t -> constant
end

let ops { ops; _ } = ops

let vars { ctx; _ } = fst ctx

let consts { consts; _ } = consts

let nvars t = Array.length (fst t.ctx)

let nconsts t = Array.length t.consts

let nsamples { samples; _ } = Array.length samples

let var_values t (var : variable) =
  let rec aux vars name n i =
    if i = n then invalid_arg "Variable is not present in the oracle"
    else
      let ((var : variable), vl) = vars.(i) in
      if var.name = name then vl else aux vars name n (i + 1)
  in
  Array.map
    (fun { vars; _ } -> aux vars var.name (Array.length vars) 0)
    t.samples

let const_values t c = Array.make (nsamples t) c.value

let out_values { samples; _ } = Array.map (fun { res; _ } -> snd res) samples

let out_var { ctx = (_, var); _ } = var

let const_of_int value =
  { name = string_of_int value;
    value = Bitvector.of_int32 (Int32.of_int value)
  }

let const_of_bitv32 value =
  { name = string_of_int (Int32.to_int (Bitvector.to_int32 value)); value }

let mk_var name sz =
  if sz <= 0 then invalid_arg "Variable size should be positive"
  else if String.length name = 0 then
    invalid_arg "Variable name should be non-empty" ;
  { name; sz }

let random_var t =
  let vars = fst t.ctx in
  vars.(Random.int (nvars t))

let random_const t = t.consts.(Random.int (Array.length t.consts))

let to_int json = int_of_string (to_string json)

let to_string json = String.lowercase_ascii (to_string json)

let get_sample_input json =
  json |> to_assoc
  |> List.map (fun (_, t) ->
         let var =
           { name = to_string (member "location" t);
             sz = to_int (member "size" t) * 8
           }
         in
         let value =
           Bitvector.extend
             (Bitvector.of_hexstring (to_string (member "value" t)))
             var.sz
         in
         (var, value))
  |> Array.of_list

let get_sample_output json =
  let json = member "0" json in
  let var =
    { name = to_string (member "location" json);
      sz = to_int (member "size" json) * 8
    }
  in
  let hex_val = to_string (member "value" json) in
  (var, Bitvector.extend (Bitvector.of_hexstring hex_val) var.sz)

let get_sample json =
  { vars = get_sample_input (member "inputs" json);
    res = get_sample_output (member "outputs" json)
  }

let get_samplings json =
  json |> member "sampling" |> to_assoc
  |> List.map (fun (_, t) -> get_sample t)
  |> Array.of_list

let get_meta_inputs json =
  json |> member "initial" |> member "inputs" |> to_assoc
  |> List.map (fun (_, t) ->
         { name = to_string (member "location" t);
           sz = to_int (member "size" t) * 8
         })
  |> Array.of_list

let get_meta_output json =
  let json = json |> member "initial" |> member "outputs" |> member "0" in
  let name = to_string (member "location" json) in
  let size = to_int (member "size" json) * 8 in
  mk_var name size

let get_meta json = (get_meta_inputs json, get_meta_output json)

let get_ops json =
  try Some (List.map (fun x -> to_string x) (to_list (member "ops" json)))
  with Type_error (_, _) -> None

let mk_consts min max =
  Array.init (1 + max - min) (fun i -> const_of_int (min + i))

let print t =
  let printf = Printf.printf in
  let print_context { ctx = (in_vars, out_var); _ } =
    printf "Variables:\n" ;
    Array.iter (fun (v : variable) -> printf "\t%s, %d\n" v.name v.sz) in_vars ;
    printf "\n\t%s, %d\n" out_var.name out_var.sz
  in
  let print_sampling t =
    printf "Samples:\n" ;
    Array.iter
      (fun { vars; res } ->
        Array.iter
          (fun ((v : variable), vl) ->
            printf "\t%s: %s\n" v.name (Bitvector.to_hexstring vl))
          vars ;
        printf "\tResult: %s\n\n" (Bitvector.to_hexstring (snd res)))
      t.samples
  in
  print_context t ;
  print_sampling t

let of_json ~filename : (module ORACLE) =
  let json = Yojson.Basic.from_file filename in
  ( module struct
    let oracle =
      { ctx = get_meta json;
        consts = mk_consts 1 1;
        samples = get_samplings json;
        ops = get_ops json
      }

    let nvars () = nvars oracle

    let nconsts () = nconsts oracle

    let nsamples () = nsamples oracle

    let var_values var = var_values oracle var

    let const_values const = const_values oracle const

    let out_values () = out_values oracle

    let out_var () = out_var oracle

    let random_var () = random_var oracle

    let random_const () = random_const oracle

    let print () = print oracle

    let vars () = vars oracle

    let consts () = consts oracle

    let ops () = ops oracle

    let const_of_int = const_of_int

    let const_of_bitv32 = const_of_bitv32
  end : ORACLE )

(*
let print_samples values results =
    let _ = Printf.printf "f(" in
    for i = 0 to (Array.length values) -1  do
        Printf.printf "%d, " (Array.get values i) ;
    done;
    let _, resval = results in
    Printf.printf ") = %d\n" (Int32.to_int (Bitvector.to_int32 resval))
*)
