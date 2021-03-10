# What is Michelson

Michelson is the domain-specific language used to write smart contracts on the Tezos blockchain.

* stack based
* strongly typed

Michelson was designed to facilitate formal verification, allowing users to prove the properties of their contracts.

# Why Michelson

At first sight, Michelson doesn't include many features like polymorphism, closures, or named functions. Its stack is not always easy to deal with, there is no standard library.

Tezos takes a slightly different view from Ethereum regarding the role of smart contracts. We think of our platform more as a way to implement pieces of business logic than as a generic "world computer".

>**tips: bytecode**
>字节码，通常指的是已经经过编译器，但与特定机器码无关，需要直译器转译后才能成为机器码的中间代码。字节码不像源码可以直接让人阅读，而是编码后的特殊序列。字节码主要是为了实现特定软体运行和软体环境，与硬件环境无关。

Michelson is designed as a readable compilation target, though it can be hand written. The goal is that even the output of a compiler can be understood.

with a lower-level bytecode, you usually need confidence in both your program and the compiler toolchain. With Michelson you can more easily check over and verify properties of the program that is actually executed. using a higher-level bytecode also simplifies the process of proving properties about the compiled output.

**Programs written in Michelson can be reasonably analyzed by SMT solvers and formalized in Coq without the need for more complicated techniques like separation logic.**

Similarly, the restrictions imposed by the forced indentation and capitalization ensure that the source cannot be obfuscated with indentation and alignment tricks.

>type-soundness 类型健全

Our current implementation of Michelson is based around an OCaml GADT, which we have used to verify the type-soundness of the language.Additionally, the implementation of a stack based language maps directly to the semantics. The same is not true for any efficient implementation of the lambda-calculus.

There have also been two formally verified implementations of Michelson, one in Coq and one in F*.

One Main Advantage of Tezos:

The system is amendable. ????????

**So, why Michelson? To provide a straightforward platform for business logic, to provide a readable bytecode, and to be introspectable. Olin Shivers was very fond of saying that one should always use a "tool small enough for the job". Michelson has been carefully designed to be that tool**.

## Property of "stack-based"

Every Michelson contract is a list of instructions that follow each other. These instructions are in a precise order and are executed in the order they're written in.

Every instruction will manipulate the stack in some way. The stack works on a last in, first out basis: if you want ot access a piece of data that is not at the top of the stack, you must first deal with the ones above it.

There main concepts must be remembered when coding in Michelson:

* New data goes on the top of the stack
* the data in the stack only become accessible when they are at the top of the stack (or in the second position for some operations, as described below)
* the order in which the data are processed goes from the top of the stack to the bottom.
### The PUSH operation

Note that there may already be data in the stack, in which case the new value will be put on the top of them. This is how you push new data in michelson:

PUSH value-type value

e.g. PUSH int 2, PUSH string "tezos"

# The Michelson Smart Contract structure

A smart contract in Michelson displays a simple structure made of three components:

* The type of the expected parameter
* The type of the storage
* The Michelson code

`

parameter parmeter-type;

storage storage-type;

code{

...

}

`

































