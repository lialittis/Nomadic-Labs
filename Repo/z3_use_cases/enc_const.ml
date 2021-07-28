open Z3util
open Z3util.Z3Ops

type t =
  { (*program*)
    p : Michelson_value_sampler.node;
    (*target program*)
    tp : Michelson_value_sampler.node option;
    (*candidates of instructions *)
    (* cis : Instruction.t list;*)
    kt : Z3.Expr.expr
  }

let size = ref 3

let sort = ref (bv_sort !size)

let mk_input_vars =
  let const s = Z3.Expr.mk_const_s !ctxt s !sort in
  List.init 3 ~f:(fun i -> const ("x_" ^ Int.to_string i))
