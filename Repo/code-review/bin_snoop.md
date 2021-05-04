# tezos-snoop


## Summary line

It is a benchmark and inference tool.

## Overview

- The purpose of `tezos-snoop` is to provide a CLI to the library
  `tezos-benchmark` and to the various benchmarks defined througout
  the code. It also provides means to display benchmark and inference
  results and to generate reports.

## Implementation Details
- The entrypoint is in the file `main_snoop.ml`.
- The modules `Cmdline` and `Commands` contain respectively type
  definitions and `tezos-clic` command definitions.
- The module `Dep_graph` allows to compute a dependency graph for
  benchmark workloads (A benchmark `Y` depends on a benchmark `X` when the
  predictive model associated to the `Y` refers to variables solved by `X`).
- The module `Display` allows to construct plots.
- The module `Report` allows to generate reports in the `latex` language.
- The `latex` sub-library is a think abstraction over latex documents.

## Structure of this program
```
.
├── cmdline.ml
├── commands.ml
├── dep_graph.ml
├── display.ml
├── display.mli
├── dune
├── dune-project
├── latex
│   ├── dune
│   ├── dune-project
│   ├── latex.opam
│   ├── lib
│   │   ├── dune
│   │   ├── latex_pp.ml
│   │   └── syntax.ml
│   └── test
│       ├── dune
│       └── test.ml
├── main_snoop.ml
├── README.md
├── report.ml
└── tezos-snoop.opam
```

## My Goal

To understand how to generate input values for scripts of Smart Contracts.

## Code Review

### Sub-library : latex

#### syntax.ml

- defines the types of all the elements of latex needed.
- several functions to deal with the variables

**Types**

t, section, section_content, text, blob, style, image, image_size, caption, table, row,
table_spec, spec, 

**Functions**

spec_width, row_width, map_string, map_string_section, 
map_string_section_content, map_text, map_string_blob, map_string_row

#### latex_pp.ml

- defines the recursive functions to construct the latex report
- uses mainly Format.fprintf funtion
- prints different parts of report 
- uses Format module

**Functions**

1. pp: Format.formatter -> r -> unit

2. pp_preamble fmtr title

formats the title to the long format string,
where *fmtr* is a Format.formatter.

3. pp_conclusion fmtr

formats a conclusion.

4. pp_section : Format.formatter -> section -> unit

formats the section with the type *Section(name, contents)*

5. pp_section_content: Format.formatter -> section_content -> unit

formats the content of sections,
where the content could be type of Text, Table or Figure

6. pp_image_size : Format.formatter -> image_size option -> unit

formats the image_size according to the type of it,
which could be None or Some(Width_cm i)

7. pp_text : Format.formatter -> text -> unit

format the text

8. pp_blob : Format.formatter -> blob -> unit

9. pp_table

10. pp_table_spec

11. pp_spec

12. pp_row

13. pp_cell_text

14. pp_cell_blob

#### test file

Open the modules of Latex and Syntax, then construct the document with type t and 
also necessary variables with corresponding types, for examples, section, contents
, table, etc.

Use Latex_pp.pp_document to print out the latex document.


## New direction

> If you want to generate Michelson terms, you will have to use this [MR](
https://gitlab.com/tezos/tezos/-/merge_requests/2801)

> It adds Michelson samplers to the protocol, located in `src/proto_alpha/lib_benchmark`, 
and also adds a command to tezos-snoop to generate Michelson terms, just type in 
`./tezos-snoop man alpha michelson generate` for more info.

### Method

```
Command for generating random Michelson code and data:
  alpha michelson generate <TERMS-COUNT> terms of kind <{data|code}> in <FILENAME> 
  [--min-size <int>]
      [--max-size <int>] [--burn-in <int>] [--seed <int>]
    Michelson generation
    <TERMS-COUNT>: Number of terms to generate
    <{data|code}>: Kind of term to generate
    <FILENAME>: File where to save Michelson terms
    --min-size <int>: Lower bound for target size of terms
    --max-size <int>: Lower bound for target size of terms
    --burn-in <int>: Burn-in multiplier
    --seed <int>: RNG seed
```


## Michelson data \& code

The Michelson generation framework consists of two phases:
- a Mikhailsky sampler (Mikhailsky = Michelson with typed holes)
- a completion function vonverting Mikhailsky terms to Michelson terms

The Mikhailsky sampler is Monte-Carlo based, ie the sequence of samples is obtained 
by running a particular Markov chain over Mikhailsky terms for a long time and recording 
the encountered terms after an initial exploration phase (called burn-in). This Markov 
chain takes as input a Mikhailsky term and returns a probability distribution over the 
possible next Mikhailsky terms.

The Markov chain is built according to the Metropolis-Hastings algorithm, which takes two
inputs:
- a Markov kernel called the "proposal"(which intuitively corresponds to some blind, 
uniform exploration),
- an unnormalized target distribution (the "objective" function, guiding the base 
exploration)

and returns a new Markov kernel with the property that there exists a unique stationary 
distribution to which it converges, and that distribution is the normalized target 
distribution.


# New New direction

Working for optimizer for Michelson based on some Machine Learning Tools, at some points, I need to provide some inputs and outputs from a given scripts. Given a script, I need to look at the type of storage and parameter, and generate input values randomly based on the types.

## Focus on the filter method. 

### rules.ml

```ocaml
let evaluate_guard_monadic guard path =
  let open Inference.M in
  match guard with
  | No_cnstrnt ->
      return ()
  | Data_cnstrnt {cnstrnt = base_type_constraint; fresh} -> (
      add_fresh_data_variables fresh
      >>= fun () ->
      get_data_annot path
      >>= fun res_opt ->
      match res_opt with
      | None ->
          assert false
      | Some type_of_expr ->
          Inference.unify_base type_of_expr base_type_constraint
          >>= fun () ->
          Inference.instantiate_base type_of_expr >>= fun _ -> return () )
  | Instr_cnstrnt {cnstrnt = {bef = pre; aft = post}; fresh; fresh_stack} -> (
      (* Add base fresh type variables *)
      add_fresh_variables fresh base_repr cmp_repr
      >>= fun () ->
      add_fresh_stack_variables fresh_stack
      >>= fun () ->
      get_instr_annot path
      >>= fun res_opt ->
      match res_opt with
      | None ->
          assert false
      | Some {bef; aft} ->
          Inference.unify pre bef
          >>= fun () ->
          Inference.unify post aft
          >>= fun () ->
          Inference.instantiate bef
          >>= fun _bef -> Inference.instantiate aft >>= fun _aft -> return () )
```

guard is the type of type_constraint. 


## REF

http://stengah.free.fr/snoop/arch.html#michelson-data-code









