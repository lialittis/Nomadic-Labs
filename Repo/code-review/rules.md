# rules.ml


To analyse the work flow of the function of *filter matches*:

Guard argument is given by guarded_replacement.type_constraint.
And guarded_replacement is given by rule.replacement,
where rule is given by rules.

The rewriting function needs this argument of rules, which is setted by
Module Sampler, which takes a Module of Sampler_parameter_sig named P.

Sampler.Make P <- Generator.MCMC <- rules = Rewrite_rules.rules X.rng_state 
<- Rules.Data_rewrite_leaves (X.Sampler) <- rules defined here

So, the result is that I cannot change the rules.

Here is the idea and the problem:

The rules are rewrited by some random state, so the type_constraint is kind of
random. What I did is define a Data_cnstrnt according to my target_type and then 
get the type of the expression by get_data_annot Path.root and unify.base it with 
target_type. 

The question is that, I am not sure about the meaning of argument fresh, and I am
not sure that if I should remain the guard.

How could the matches be unified by target_type and random state at the same time ?
- NO Possibilities

How could the 


* What is fresh ?

When a rewriting rule is defined, a precondition for the application of the rule 
can be specified. If this precondition is not met, the rule is not applied.
One of these precondition is that the type of the subterm being rewritten is compatible with a particular type. The way this works is that the type of the subterm is unified with the type of the constraint.
8:33
For instance, the constraint can be a type of the form "list(int)", then the rule will be only applied at a subterm if that subterm has a type compatible with "list(int)"
8:35
Now, types have variables, so it is possible to say "list(anything)". The way this is encoded is by using special "fresh" variables, which means that they must be chosen to never intersect with the variables that could appear in the term. The fresh field contains the free variables that appear in the type constraint.
8:35
In my program, type variables are represented as integers, hence fresh is an int list
8:37
Tell me if it is not clear
8:38
Do you need this to solve your problem, though? These type constraints are local to the subterm being replaced, not the global type

