module type Oracle = sig
  type variable = { name : string; size : int }

  type constant = { name : string; var : int }

  type samples = { inputs : (variable * int) array; out : variable * int }

  type ctx = variable array * variable (* inputs * output *)

  val mk_consts : int -> int -> constant array

  (* mk_var *)
  (*  val mk_var : string -> int -> variable
*)
  (* random_select input and const *)
  val random_input : ctx -> variable

  val random_const : constant array -> constant

  (* get value of variable *)
  val val_of_variable : variable -> samples array -> int array

  val const_values : constant -> int array

  (* initialization *)
  val nvars : int

  val nconsts : int

  (*  val nsamples : int
*)
  val sample_ctx : ctx

  val sample_samples : samples

  (* type for samples variables *)
end

module Mk_Oracle () : Oracle

module Oracle : Oracle

type unop_t

type t

module type Input = sig
  val input : int

  val output : int
end

module Mk_Input () : Input

module type Unop = sig
  val rand : unit -> unop_t

  val apply : unop_t -> int -> int
end

module Mk_Unop () : Unop

module type Template = sig
  val get_values_from_t : t -> int array

  val random_unop : unit -> t
end

module Mk_Template () : Template
