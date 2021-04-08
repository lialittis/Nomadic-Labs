# Readme

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

2. What is the labelled variable *check* ?

3. What is *expand_all*?

The labelled parameter *source* is type of String, and the second labbelled parameter *original* is the *Micheline_parser.node*, and the output is the *Micheline_parser.parsing_result*. (*To verify*)


(* expands : expression with macros fully expanded *)

4. The abstract syntax tree of Micheline expressions

The first parameter -> contain locations, can also embed custom data;
The second parameter -> the type of preimitive names.

5. What is the difference between expanding expressions and unexpanding expressions ?

expands means expression with macros fully expanded.

## TODO

### parsering part

Expanding expressions ; 


### dune part

I. *dune-project* file


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

III. install

dune build @install

IV. build executable file

dune build <name>.exe





















