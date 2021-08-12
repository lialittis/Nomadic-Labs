# Heuristics Method

The key function is `search`.

with the inputs of two modules :

- Dist : distance calculation

- Oracle : samples information

- maxi:

- perturbate : execute perturbations for solution we have, the number of cuts is
2(could be settled); the cut method is from Module of Mutator (from the tree module)

-----------------------------------
let's move to cut method:

the input argument is type t, which is actually a tree.

According to the type of the node of the tree:

case : Var or Const => create a random terminal
case : Unop => create a random terminal, and use the op to evaluate it
case : Binop or Triop => consider the rank of the nodes

The logic should be, generating an integer(id = 1 + Random.int size_of_tree), if this
id is equal to the size of the current node, all the tree will be cutted and setted 
as random terminal, if not, then do the recursion of cutting for the child-node and also
generate the corresponding operator with the id and the return of the cutting node.

But be careful, if there are multiple terms for the operator, we sould deal with them 
seperately.

Questions: why the cut numbers is 2 ?
-----------------------------------

