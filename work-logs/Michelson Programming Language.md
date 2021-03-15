# Michelson | the language of Tezos

## What is Michelson

Michelson is the domain-specific language used to write smart contracts on the Tezos blockchain.

* stack based
* strongly typed

Michelson was designed to facilitate formal verification, allowing users to prove the properties of their contracts.

## Why Michelson

At first sight, Michelson doesn't include many features like polymorphism, closures, or named functions. Its stack is not always easy to deal with, there is no standard library.

Tezos takes a slightly different view from Ethereum regarding the role of smart contracts. We think of our platform more as a way to implement pieces of business logic than as a generic "world computer".

>**tips: bytecode**
>字节码，通常指的是已经经过编译器，但与特定机器码无关，需要直译器转译后才能成为机器码的中间代码。字节码不像源码可以直接让人阅读，而是编码后的特殊序列。字节码主要是为了实现特定软体运行和软体环境，与硬件环境无关。

Michelson is designed as a readable compilation target, though it can be hand written. The goal is that even the output of a compiler can be understood.

With a lower-level bytecode, you usually need confidence in both your program and the compiler toolchain. With Michelson you can more easily check over and verify properties of the program that is actually executed. Using a higher-level bytecode also simplifies the process of proving properties about the compiled output.

**Programs written in Michelson can be reasonably analyzed by SMT solvers and formalized in Coq without the need for more complicated techniques like separation logic.**

Similarly, the restrictions imposed by the forced indentation [强制缩进] and capitalization [大写] ensure that the source cannot be obfuscated with indentation and alignment tricks.

>type-soundness 类型健全

Our current implementation of Michelson is based around an OCaml GADT, which we have used to verify the type-soundness of the language. Additionally, the implementation of a stack based language maps directly to the semantics. The same is not true for any efficient implementation of the lambda-calculus.

There have also been two formally verified implementations of Michelson, one in Coq and one in F*

* One Main Advantage of Tezos: The system is amendable. [可修改的] ????????

**So, why Michelson? To provide a straightforward platform for business logic, to provide a readable bytecode, and to be introspectable. Olin Shivers was very fond of saying that one should always use a "tool small enough for the job". Michelson has been carefully designed to be that tool**.

## Property of "stack-based"

Every Michelson contract is a list of instructions that follow each other. These instructions are in a precise order and are executed in the order they're written in.

Every instruction will manipulate the stack in some way. The stack works on a **last in, first out** basis: if you want to access a piece of data that is not at the top of the stack, you must first deal with the ones above it.

There main concepts must be remembered when coding in Michelson:

* New data goes on the top of the stack

* the data in the stack only become accessible when they are at the top of the stack (or in the second position for some operations, as described below)

* the order in which the data are processed goes from the top of the stack to the bottom.

### The PUSH operation

Note that there may already be data in the stack, in which case the new value will be put on the top of them. This is how you push new data in michelson:

$ PUSH value-type value

e.g. PUSH int 2, PUSH string "tezos"

## The Michelson Smart Contract structure

**A smart contract in Michelson displays a simple structure made of three components:**

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

除此结构外，在Michelson编写智能合约时，还必须牢记两条规则：

1. 在执行代码时，自动将包含参数和存储器的对推入桟中，如果没有参数，就用单位代替。
2. 代码必须始终返回一对，它包含操作列表和更新的存储，当堆栈中只剩下此类对时，执行就会停止。

### 一个简单的 Michelso 智能合约

现在我们已经了解了 PUSH 以及 Michelson 中智能合约的结构，让我们来写一个。

对于这个合约，我们将写一个「 Hello world」合约，并将一个字符串保存到存储中：

执行这段代码后，会发生以下情况：

1、参数 unit 表示所传递的参数为 unit 类型（基本上是无参数）。

2、storage string 表示合同的存储类型为 string。

3、DROP 是一种操作代码，用于删除堆栈顶部的任何内容。我们之前说过，一个带有参数和存储空间的对在开始时会自动包含在栈顶，我们不打算使用它，可以将其删除。

4、PUSH 将一个值放到栈顶，这里是字符串「 Hello world」。

5、NIL 是一种操作码，它将指定类型的空列表 (此处操作) 添加到堆栈的顶部。

6、PAIR 将两个元素放在堆栈顶部，创建一个包含这两个元素的新对，然后将其推回堆栈中。

注意：每条指令都以分号结尾（最后一条指令是可选的）。

## Difference between Michelson and EVM

### Stack Machines

The most notable difference between Michelson and EVM is that Michelson is written in a human legible text format, whereas EVM operations are represented as bytes. For example, if you look up the opcode table for the EVM you'll see that the opcode 01 takes two numbers (off the top of the stack) and adds them together. The equivalent operation in Michelson is written as ADD.

Example: we have a stack like this
> 20 : 7 : 13 : 45 : []
where : is our element separator and [] indicates the bottom of the stack.

In the illustration we apply the operation ADD to the stack, which has the following definition:

> ADD / a : b : S => (a + b) : S

In plain English, this definition says "The ADD operation removes the top two elements of the stack (a and b in the definition) and then puts the element (a + b) back onto the top of the stack:

ADD / 20 : 7 : 13 : 45 : [] =>
    (20 + 7) : 13 : 45 : [] =>
          27 : 13 : 45 : []


All computation with Michelson works similarly based on this process of stack mutation. We used ADD in the above example, but we could also mutate the stack with other **arithmetic operations** like subtraction or multiplication, or **logical operations** like NOT, AND, OR. We can directly manipulate the stack by explicitly pushing data onto it, or by swapping around or duplicating elements. We have **control flow structures** like *LOOP* or *IF*. We can perform some **cryptographic operations** like hashing or checking signatures, and we can interact with the blockchain by initiating token transfers or creating accounts. Michelson has lots of different operations.

### Types

The second major difference between Michelson and EVM, is that Michelson data elements are typed.
Broadly speaking, a type is a piece of information that constrains the possible things that can be done with a given data value. If the value 1 has the type int (for integer), then we know that we can safely perform numerical addition on it, but that we can't safely perform list indexing.
For the value "foobar" with type string, the situation is reversed. Adding a number to a string 1 + "foobar" is not well-defined, because **addition is an operation on integers (in most languages, some languages overload the + operator to mean numerical addition when its arguments are numbers and concatenation when its arguments are strings)** 

Types are useful because they allow the Michelson interpreter to exclude programs that may have problematic behaviour. For example, with the natural number type, nat, attempting to subtract a larger nat from a smaller nat like 4 - 5 will result in an error. The programmer then can determine during testing whether this error is the result of an undesired operation, incorrect arguments, or whether the type signature of the values ought to be changed. **The crucial thing is that this error occurred early on and prevented the program from running at all, rather than for the possible bug to slip past testing unnoticed only to cause issues later on in production. Generally speaking, types allow the programmer to communicate their intentions in more detail to the machine, and allows the machine to communicate to the programmer when execution deviates from those intentions.**

## How to set up a Michelson environment

Okay, now that we've covered a little bit of the theory of how Michelson works, let's get our hands dirty with some code.

### Installing the Tezos 



### Hello Tezos

Open up your favorite editor and write the following program helloTezos.tz in the same directory you put the babylonnet.sh script.




## How to use Tezos

### The Binaries

After a successful compilation, you should have the following binaries:

- tezos-node: the tezos daemon itself;

- tezos-client: a command-line client and basic wallet;

- tezos-admin-client: administration tool for the node;

- tezos-{baker,endorser,accuser}-: daemons to bake, endorse and accuse on the Tezos network (see How to run Tezos);

- tezos-signer: a client to remotely sign operations or blocks (see Signer);

The daemons are suffixed with the name of the protocol they are bound to. For instance, tezos-baker-006-PsCARTHA is the baker for the Carthage protocol. See also the Node Protocol section below.












## Thanks

https://gitlab.com/camlcase-dev/michelson-tutorial/tree/master/01

http://tezos.gitlab.io/introduction/howtouse.html





























