type variable = private { name : string; sz : int }

type constant = private { name : string; value : Bitvector.t }

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

val of_json : filename:string -> (module ORACLE)
