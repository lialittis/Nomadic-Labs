open Protocol
open Michelson_interpreteur
open Michelson_value_sampler
open Michelson_v1_primitives

(* Input parameter parsing *)
let _ =
  if Array.length Sys.argv < 3 then (
    Format.eprintf
      "Executable expects random seed and target type on input\n%!" ;
    exit 1 )
  else Random.init (int_of_string Sys.argv.(1))

let global_type_string = Sys.argv.(2)

let output_as_string = print_prims global_type_string

(* [TEST] test default *)
let test_defaut () =
  test_context ()
  >>=? fun ctx ->
  (* Run script with a parameter of wrong type *)
  run_script
    ctx
    "{parameter unit; storage unit; code {CAR ; NIL operation; PAIR }}"
    ~storage:"Unit"
    ~parameter:"Unit"
    ()
  >>= function
  | Ok _ ->
      Format.printf "test_default_script : success\n" ;
      return_unit
  | Error errs ->
      Format.printf "test_default_script : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

let output = test_defaut ()

(* [ENDTEST]*)

let test_interpreter parameter =
  test_context ()
  >>=? fun ctx ->
  run_script
    ctx
    "{parameter int; storage int; code { DROP ; PUSH int 1; NEG ; NIL \
     operation ; PAIR}}"
    ~storage:"1"
    ~parameter
    ()
  >>= function
  | Ok res ->
      Format.printf "test_int_script : success\n" ;
      return @@ res
  | Error errs ->
      Format.printf "test_int_script : fail\n" ;
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_error errs

(* apply sampler result to the interpreter *)
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
