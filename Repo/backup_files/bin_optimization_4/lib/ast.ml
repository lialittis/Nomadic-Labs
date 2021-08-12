let random_select (omega : 'a array) =
  let index = Random.int (Array.length omega) in
  omega.(index)

(****************************************)
(*Module Oracle Part*)
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

(*
let gen_oracle () : (module Oracle) =
*)
module Mk_Oracle () : Oracle = struct
  type variable = { name : string; size : int }

  type constant = { name : string; var : int }

  (* support that, the inputs are int array, the output is int *)

  type samples = { inputs : (variable * int) array; out : variable * int }

  (* type for samples variables *)
  type ctx = variable array * variable (* inputs * output *)

  (* initialization *)
  let nvars = 2

  let nconsts = 1

  let nsamples = 1

  (* mk_consts *)
  let const_of_int value = { name = string_of_int value; var = value }

  let mk_consts min max =
    Array.init (1 + max - min) (fun i -> const_of_int (min + i))

  (* mk_var *)
  (*  let mk_var name size = { name; size }
*)
  (* random_select input and const *)
  let random_input (ctx : ctx) =
    let vars = fst ctx in
    let var = random_select vars in
    print_string "variable chosen :" ;
    print_endline var.name ;
    var

  let random_const (consts : constant array) =
    let const = random_select consts in
    print_string "random const chosen :" ;
    print_int const.var ;
    print_endline "" ;
    const

  (* get value of variable *)
  let val_of_variable (var : variable) samples =
    let rec aux vars name n i =
      if i = n then invalid_arg "Variable is not present in the oracle"
      else
        let ((var : variable), (vl : int)) = vars.(i) in
        if var.name = name then vl else aux vars name n (i + 1)
    in
    Array.map
      (fun { inputs; _ } -> aux inputs var.name (Array.length inputs) 0)
      samples

  let const_values c = Array.make nsamples c.var

  let sample_ctx : ctx =
    ( Array.of_list [{ name = "x1"; size = 1 }; { name = "x2"; size = 1 }],
      { name = "x3"; size = 1 } )

  let sample_samples =
    { inputs =
        Array.of_list
          [({ name = "x1"; size = 1 }, 10); ({ name = "x2"; size = 1 }, 20)];
      out = ({ name = "x3"; size = 1 }, -20)
    }

  (* *)
end

(****************************************************************)

type unop_t = Minus | Not

module Oracle = Mk_Oracle ()

type node =
  | Var of Oracle.variable
  | Cons of Oracle.constant
  | Unop of unop_t * t

and t = { node : node; size : int; (* number of nodes *) vals : int array }

module type Input = sig
  val input : int

  val output : int
end

(* a simple input module *)
(*let get_Input () =
  ( module struct
    let input = 10

    let output = -10
  end : Input )

let input_example = get_Input ()
*)
module Mk_Input () : Input = struct
  let input = 10

  let output = -10
end

module Input = Mk_Input ()

module type Unop = sig
  val rand : unit -> unop_t

  val apply : unop_t -> int -> int
end

module Mk_Unop () : Unop = struct
  let number = 2

  let of_int = function 0 -> Minus | 1 -> Not | _ -> assert false

  let omega = Array.init number of_int

  (* random select operators *)
  let rand () = random_select omega

  (* apply the operation to variable *)
  let apply op x =
    match op with
    | Minus ->
        print_endline "operator : minus" ;
        x - 1
    | Not ->
        print_endline "operator : not" ;
        Int.neg x

  (*let of_vals t = t.vals*)
end

module type Template = sig
  val get_values_from_t : t -> int array

  val random_unop : unit -> t
end

module Mk_Template () : Template = struct
  module M_Unop = Mk_Unop ()

  let mk_unop op t =
    let size = match op with Minus | Not -> t.size + 1 in
    { node = Unop (op, t);
      size;
      vals = Array.map (fun x -> M_Unop.apply op x) t.vals
    }

  let mk_var var =
    { node = Var var;
      size = 1;
      vals = Oracle.val_of_variable var [| Oracle.sample_samples |]
    }

  let mk_const c = { node = Cons c; size = 1; vals = Oracle.const_values c }

  (* generate a random t from variable and constant *)
  let random_terminal () : t =
    let nvars = Oracle.nvars in
    let nconsts = Oracle.nconsts in
    let choice = Random.int (nvars + nconsts) in
    if choice < nvars then mk_var (Oracle.random_input Oracle.sample_ctx)
    else mk_const (Oracle.random_const (Oracle.mk_consts 1 2))

  (*
  let random_unop op =
    match op with
    | Some operator -> mk_unop operator (random_terminal ())
    | None -> mk_unop (M_Unop.rand ()) (random_terminal ())
*)
  let get_values_from_t t = t.vals

  let random_unop () = mk_unop (M_Unop.rand ()) (random_terminal ())
end
