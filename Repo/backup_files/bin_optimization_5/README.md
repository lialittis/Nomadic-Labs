# Readme


## How to execute ?

./../../_build/default/src/bin_optimization/main.exe <-option> <filename> [<-n>] [<-r>] [<-o>] [<-json>]
  -option [sample | optimize] : option to generate I/O relations or optimize one Michelson Program
  -n number of samples to nenerated (sample), default value is 10
  -r the json file contaioning I/O relationships (optimize)
  -o output file for relationship (sample)
  -json if the output file in json format, default value is false
  -help  Display this list of options
  --help  Display this list of options

##############################################################################################

## Micheline\_parser

The parser I want to use is built in */src/protocol_alpha/lib_client/Michelson_v1_parser*;

### Code Review

- Used Modules: *Protocol, Tezos_micheline, Micheline_parser, Micheline*
- Key functions: 
	- *parse_expression ?check source*
	- *expand_all ~source ~original*

**To understand:**

1. The type *parsed*:

```ocaml
type parsed = {
  source : string;
  unexpanded : string canonical;
  expanded : Michelson_v1_primitives.prim canonical;
  expansion_table : (int * (Micheline_parser.location * int list)) list;
  unexpansion_table : (int * int) list;
}
```

where *Michelson_v1_primitives.prim* is defined in */protocol_alpha/lib_protocol/michelson_v1_primitives*, and it offers method to transfer the prims to strings, which is *strings_of_prims*

2. What is the labelled variable *check* ?


3. What is *expand_all*?

The labelled parameter *source* is type of String,
and the second labbelled parameter *original* is the *Micheline_parser.node*,
and the output is the *Micheline_parser.parsing_result*. (*To verify*)

(* expands : expression with macros fully expanded *)

4. Micheline.node 

It is the abstract syntax tree of Micheline expressions:
The first parameter -> contain locations, can also embed custom data;
The second parameter -> the type of preimitive names.

In the michelson_v1_macros, the second paremeter is *string*.


5. What is the difference between expanding expressions and unexpanding expressions ?

expands means expression with macros fully expanded.


6. Type of the parseing\_result ?

type 'a parsing_reslut = 'a * error list

In the michelson_v1_parser, the 'a type is defined as *parsed*, as in 1.


7. canonical

type 'p canonical

## Translator & Interpreter

### Code Review


- Use modules : *Script_ir_translator, Script_ir_interpreter*
- Key functions : ?

This translator we use is defined in */src/protocol_alpha/lib_protocol/*,
however it takes time to understand the methods involved.

**To understand**

1. *Script_repr* 

defines *location*, *expr*, *node*, *error*, etc.

2. *Script_int_repr*

defines type num as interger and defines some methods to manipulate the num.

3. Read the code in the test file of *interpretation.ml*.

4. *Data_encoding* Module

which is constructed by Nomadic Labs.

In the function run_script function, contract_expr and storage_expr (with type of Script.Expr) are implemented to type lazy_expr, which is realized by `Michelson_v1_primitives.prim Micheline.canonical Data_encoding.lazy_t`.

### Questions 

1. Why there is *Script* module and *Script_int* module, while there is not *script.ml* or *script_int.ml* ?

Answer: module could be defined in the code by keyword.

2. What does this function mean?

```
let ( >>=?? ) x y =
  x
  >>= function
  | Ok s ->
      y s
  | Error errs ->
      Lwt.return
      @@ Error (List.map (fun x -> Environment.Ecoproto_error x) errs)
```

Answer: Manod Programming

3. How to correct the fault, once the parameter or the storage is not Unit ?



## TODO

### parsering part

- [x] Expanding expressions ; 

### interpreter part

- [x] test the interpreter by small strings ;
- [ ] fix the bug of reader
- [ ] what if the type of storage or parameter ;

### dune part

I. *dune-project* file

- [x] construct
- [ ] optimize

II. *dune* file

	1. executable file name

	2. <package>.opam

	When a <package>.opam file is present, dune will know that the package named <package> exists.

	It will know how to construct a <package>.install file in the same directory to handle installation via opam.

	To build everything that is installable in a workspace, run at the root:

	$ dune build @install

	Declaring a package this way will allow you to add elements such as libraries, executables, documentation, … to your package by declaring them in dune files.

	Such elements can only be declared in the scope defined by the corresponding <package>.opam file. Typically, your <package>.opam files should be at the root of your project, since this is where opam pin ... will look for them.

	Note that <package> must be non-empty, so in particular .opam files are ignored.

- [x] construct
- [ ] optimize, for example, reduce the packages

III. install

dune build @install

IV. build executable file

dune build <name>.exe

dune exec ./<name>.exe


## Michelson Sampling and Generation

python





