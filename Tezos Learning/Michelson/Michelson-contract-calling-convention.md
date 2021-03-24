# How the contract works

All Michelson smart contracts are functions that take two arguments, an input parameter and a storage value, and return a pair of a list of network operations and a storage value. The storage value is effectively a return value, and the list of network operations are the effects(such as a transcation, delegation, or new contract creation) the contract will emit onto the Tezos network.

You can think of a contract's return value as the pair of how the contract mutates the global blockchain state and how the contract mutates its own state. 

There are many different ways to notate types signatures but here's what the Michelson type signature of a contract looks like:

> lambda (pair 'parameter 'storage) (pair (list operation) 'storage)

Personally, I prefer type notation more like Haskell's, but both signatures are equivalent:

> contract :: (Parameter p, Storage s) -> ([Operation], Storage s)

# Examples

## helloTezos intial and return types

`
 # helloTezos.tz
parameter unit;
storage string;
code { DROP;
       PUSH string "Hello Tezos!";
       NIL operation; PAIR;
     };

`

The parameter unit and storage string lines specify the types of the contract's two arguments. If we concretize the above signature for the general type of Michelson contracts, with 'parameter as  unit and 'storage as string, we get the type of our specific helloTezos.tz contract:

> lambda (pair unit string) (pair (list operation) string)

The initial stack of a Michelson contract is its argument pair `pair 'parameter 'storage'` so hellpTezos.tz starts with a stack of type:

> :: (pair unit string) : []

At the command line we can use Mockup Mode of tezos-client.

> tezos-client --mode mockup run script helloTezos.tz on storage '""' and input Unit

'""' is the command-line syntax for "", the empty string and Unit is the data constructor of the single inhabitant of the unit type. Be advised that input and parameter are synonymous here. So our initial stack has the concrete value:

> (Pair Unit "") : []

which has type

> :: (pair unit string) : []

and we want to end up with 

> ???
> :: (pair (list operation) string) : []



Fortunately, our contract is pretty short at only 4 operations, so we can walk
through the steps of this transformation here. The operations are written after
the code heading in helloTezos.tz.

If we write down this operation sequence and the initial values and types from
the previous section, we get the full state of our Michelson stack machine:

`
STATE
code  DROP; PUSH string "Hello Tezos!"; NIL operation; PAIR;
stack    (Pair Unit "") : []
type  :: (pair unit string) : []

`

### Steps description

#### DROP

The DROP operation removes (or "drops") the top element of the stack and has
the following definition (slightly rearranged from the Michelson spec)

`
code  DROP
stack    _ : S  =>  S
type  :: _ : 'A -> 'A
`

where _ is a wildcard matching any operations, values, or types
Applying this to our state, our new state is:

`
STATE
code  PUSH string "Hello Tezos!"; NIL operation; PAIR;
stack    []
type  :: []
`

#### PUSH

The PUSH operation adds a value with a certain type onto the top of the stack
and has the following definition:

code  PUSH 'a (x :: 'a)
stack S  =>  x : S
type  'A -> 'a : 'A

Our concrete instruction is PUSH string "Hello Tezos!", so the tranformation
is concretized as:

code  PUSH string ("Hello Tezos!" :: string)
stack S  =>  "Hello Tezos!" : S
type  'A -> string : 'A

which when applied gives us a new state:

STATE
code  NIL operation; PAIR;
stack "Hello Tezos!" : []
type  :: string : []


#### NIL

The NIL operation adds an empty list of a certain type onto the top of the
stack and has the following definition:

code  NIL 'a
stack S  =>  {} : S
type  'A -> list 'a : 'A

which when applied gives us a new state:

STATE
code  PAIR;
stack    {} : "Hello Tezos!" : []
type  :: list operation : string : []

#### PAIR

The PAIR operation removes the top two elements of the stack, makes a pair of
them, and pushes the pair onto the stack. It has the following definition:

code  PAIR
stack a : b : S  =>  (Pair a b) : S
type  'a : 'b : 'A -> pair 'a 'b : 'A

which when applied gives us a new state:

STATE
code
stack    (Pair {} "Hello Tezos!") : []
type  :: pair (list operation) string : []



#### End

We are now out of operations, and our stack has type pair (list operation) string : [] 
which is exactly what we wanted. Since the type matches our expected return type, 
the contract returns the values in our (pair 'operations 'storage):

storage
  "Hello Tezos!"
emitted operations


## Conclusion

This concludes Part I of our Michelson tutorial. You should now know

- the basics of how a stack machine works
- how to install the Tezos client
- how to write and execute a simple michelson contract
- what Michelson types, values and operations are and some simple examples of each one

In Part II, we will write more contracts and go through their execution
(although with less rigor), introducing more operations, types and data
constructors.

## Exercises for the reader


1. Copy the contents of helloTezos.tz to a new file helloWorld.tz, and modify it to output "Hello World!" instead of "Hello Tezos!".


2. Copy the contents of helloTezos.tz to a new file helloInput.tz, and modify it to take a string argument as input and output

"Hello <input>"..

You will need to know two additional operations to do this:

code  CAR # select left-hand of pair
stack (Pair a \_) : S  =>  a : S
type  pair 'a 'b : 'A -> 'a : 'A


code  CONCAT # string concatenate
stack a : b : S  =>  a ^ b : S
type  string : string : 'A -> string: 'A
  where
    a ^ b concatenates the end of a to the beginning of b

3. In a new file helloStorage.tz, do the same thing as exercise 2, except now the contract takes a unit as its input parameter, and outputs "Hello " concatenated to its initial storage.

e.g. if  on storage '"bar"' and input 'Unit', then it should output "Hello bar".


You will need to know one additional operation:

code  CDR # select right-hand of pair
stack (Pair _ b) : S  =>  b : S
type  pair 'a 'b : 'A -> 'b : 'A

4. In a new file helloInputAndStorage.tz, write a contract that concatenates an input string and a storage string and ouputs "Hello <input><storage>".

You will need to know:

code DUP # Duplicate the top of the stack.
stack x : S  =>  x : x : S
type  :: 'a : 'A   ->   'a : 'a : 'A

code DIP ops #  Runs code protecting the top of the stack.
stack x : S  =>  x : S'
  where    ops / S  =>  S'
type :: 'b : 'A   ->   'b : 'C
  iff   ops :: [ 'A -> 'C ]
ops can be empty {} or a list of operations like {PUSH string "Hello"; CONCAT}

You could also use SWAP instead of DIP

SWAP # Exchange the top two elements of the stack.
stack x : y : S  =>  y : x : S
type :: 'a : 'b : 'A   ->   'b : 'a : 'A

# Appendix A: Michelson Data Literal format

Outer single quotes only required for passing literals through the command line.
(adapted from https://www.michelson-lang.com/contract-a-day.html)

Type
Data

unit
Unit

nat
12312321


int
12312321


int
-12223


string
'"hello"'


bool
True


bool
False


timestamp
10000


timestamp
'"2017-08-14T18:00:21Z"'


mutez

1 (equivalent to 1/1000000 of a tez)


mutez

10000 (equivalent to one tez cent)


mutez

1000000 (equivalent to one tez)


(lambda unit unit)
{}


(lambda string string)
{PUSH string "hello"; CONCAT}


bytes
0x0001


key
'"edpktieBMrR9Df3dqzZAJpDZBXb1h188fyrQyRJcm5RH2WpbvMVR8b"'


signature
'"edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb"'


key_hash
'"tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"'


address
'"tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"'


(contract unit)

'"tz1Z1nn9Y7vzyvtf6rAYMPhPNGqMJXw88xGH"' (not a smart contract)


(contract unit)

'"KT1EM8rZNSvjRny1tqQWp4qWd6uTA9FeSFvU "' (a smart contract)


(big_map nat bool)

{} (an empty big_map)


(big_map nat bool)

{Elt 1 False} (a big_map with one element)


(big_map nat bool)

{Elt 1 False; Elt 1 True} (a big_map with two elements)


(map nat bool)

{} (an empty map)


(map nat bool)

{Elt 1 False} (a map with one element)


(map nat bool)

{Elt 1 False; Elt 1 True} (a map with two elements)


(set nat)

{} (an empty set)


(set nat)
{1; 2; 3; 4}


(list int)

{} (an empty list)


(list int)
{1; -123; 12312}


(option bool)
(Some False)


(option bool)
None


(or nat bool)
(Left 100)


(or nat bool)
(Right False)


(or (or nat unit) (or bool string))
(Left (Left 2))


(or (or nat unit) (or bool string))
(Left (Right Unit))


(or (or nat unit) (or bool string))
(Right (Left True))


(or (or nat unit) (or bool string))
(Right (Right "hello"))


(pair nat bool)
(Pair 1 True)
























