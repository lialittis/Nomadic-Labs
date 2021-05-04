# Goal

Working for optimizer for Michelson based on some Machine Learning Tools, 
at some points, I need to provide some inputs and outputs from a given scripts. 
Given a script, I need to look at the type of storage and parameter, and generate 
input values randomly based on the types.

# Contents

## Intro

- branch: snoop-proto-merge
- path: src/proto_alpha/lib_benchmark

Inside of lib_benchmark, there is one folder called test, and we see test_sampling_data.ml.

### Start by a test file : test_sampling_data.ml

- Instance some parameters of sampler
- Call data generator

### How the generator works : generators.ml

The sampler is based on Markov Chain,  the transations of Markov Chain perform of rewriting 
these terms. It is parameterd by the rewriting rules. 

It is hard to generate Michelson terms without holes.  

In the module of MCMC, 

- initial part - initial state
- energy part - to drive the state space to have the right size
- select the rewriting rules that you want to use - to ensure that the results you get 
is well typed
- in each state, there is one term with holes

### Rewrite rules

- some definition, e.g. type_constraint
- rewriting function
    - inputs : current state and rules;
    - for each rule, compute all possible matches of rule on your term;
    - for each match, check it by filter_matches;
    - for each match which is ok, perform rewriting;
    - let Markov Chain decide which rewriting you are going to pick.
- evaluate_guard_monadic function 

Note that, the rewriter can replace the whole term with the hole, it can remove part of 
the initial term.

# What we can do

In the data generation inside of the test_sampling_data.ml, we don't specifiy any type 
[TODO:add something here], to give a type we want to sample. 

This type will be used, firstly in the initial state, to add a type constraint [TODO]. 

Then, we also want to contraint the sampler, the part of selection of rules will be 
applied to reject rules that make you escape the type you give. 

This has to be hacking into the Sampler.Make: in proposal function, we need to add the 
rules that rewrite the function to use rewriting_options, by adding some parameters 
there(which is the type you want for the term) [TODO].

# In general

We will need to generalize the functor called Data, to see if we can provide a type which 
could be propagated inside of the generator's functions, to filter the data we want.
