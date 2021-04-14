# Mockup mode

Mockup mode allows easy prototyping of Tezos applications and smart contracts locally. By local we mean:

- The relevant data files sit in a directory on your computer’s local filesystem.
- These files are a lightweight emulation of the internal state of a Tezos single-node network.
- Thus, networking communications infrastructure that a node would be wrapped in, has been stripped away.
- Likewise, the consensus mechanisms that would be needed for a live blockchain with a network and multiple nodes, have been stripped away.
- The state is directly accessible and modifiable, since it’s just files on your computer.
- You don’t need (complex) setups of node-client interactions. There is no infrastructure aside from your own filesystem with a bundle of state files in a directory. It’s easy to inspect these files, modify them, and play with different configurations and protocols.


If a single sandboxed node were an apricot then mockup mode would be the apricot kernel, and we could write:

```
mockup mode = kernel_of(one sandboxed Tezos node)
```

The motivation in building mockup mode was to give our developers, who are building and testing Tezos smart contracts, an easy local environment offering a fast development cycle which needs only lightweight local state files, and which does not require a running blockchain.

## Introduction to tezos-client

It is the main tool for advanced user interaction with the Tezos blockchain.

It can prepare transactions; evaluate, typecheck and orginate contracts;
and encode/decode data when interaction with nodes. It also acts as a wallet,
allowing to sign arbitrary data - including, of course, transactions.

The mockup mode of tezos-client supports these operations (with slight limitations),
with the convenience that it does not need to be connected to a live Tezos node.
All operations are local, in the sense above.

tezos-client in mockup mode does two things to compensate for not communicating with the live network:

- It allows the user to specify — or if none are specified, it invents — dummy values for required initialisation parameters which would usually be gathered from a live node. Examples include: the head of the chain; or the client’s network identifier.
- It simulates activation of the protocol, and runs local implementations of the RPCs (Remote Procedure Calls).

## Three modes of operation

1. stateless mode:
	In this mode, tezos-client operates on its inputs and returns a value. Nothing is written to disk, and no state is preserved between calls to the client.
2. stateful mode:
	In this mode, tezos-client creates or manipulates a state on disk. The switch for this is --base-dir <directory_name>;
3. stateful asynchronous mode
	This mode adds baking. The command-line switch for this is --base-dir <directory_name> --asynchronous;

## Capabilities of mockup mode

The current implementation of mockup mode can:

- Typecheck, serialize, sign and evaluate a contract. **These features work in stateless mode.**
- Perform transactions, originations, and contract calls — mimicking sandboxed mode but without a node. **These features require a state.**
- Register delegates and bake blocks. **These features require an asynchronous state.**

## Run a mokeup client in stateless mode

**Typecheck and evaluate scripts**

* Typecheck

```
$ tezos-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
  --mode mockup typecheck script hardlimit.tz
```

`
Well typed
Gas remaining: 1039988.270 units remaining
`

* Evaluate

`
$ tezos-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
  --mode mockup run script hardlimit.tz \
  on storage '2' and input 'Unit'
`

`
storage
  1
emitted operations
  
big_map diff
`

> Note that, without --protocol option, the mockup mode will choose a protocol for you.


**Query available mockup protocols**

We can query the list of the Tezos protocols that mockup mode supports:

`
$ tezos-client list mockup protocols
`

A word on the protocol for naming protocols: **The Tezos protocol IDs above are based on hashes, but the start of each ID hints at the release name of the corresponding protocol. The three items above correspond to protocols called alpha (a development version of the Tezos protocol), Delphi, and Carthage.**

Getting these IDs matters because a Tezos blockchain requires a protocol, thus in particular setting up a mockup state requires us to choose a protocol. The list above tells us what’s available.

## Run a mockup client with state

Giving the mockup client some state allows access more of the available functionalities. In particular, given a state we can operate on it, including:

- transferring Tez cryptocurrency tokens (ꜩ),
- originating (deploying) contracts,
- importing keys, and
- querying balances or (more generally) making RPC queries on the chain’s current state.

### A useful command alias: mockup-client

A shell alias will let us call `tezos-client` with `--mode mockup` and `--base-dir /tmp/mockup`, and so save us keystrokes later:

`
$ alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'
`

```
$ mockup-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK create mockup
```

The output above confirms that:

- A mockup state data directory /tmp/mockup has been created. Data is
	- in /tmp/mockup for non-mockup-specific elements (like accounts), and
	- in /tmp/mockup/mockup for mockup-specific data (like mempool, trashpool, and context) — see asynchronous state for details).
- Five accounts have been added, and their addresses are listed.

The five accounts are called bootstrap1 to bootstrap5 (see command below). 
The reader familiar with Tezos’ sandboxed client may recognize them as the 
preconfigured bootstrap1 to bootstrap5 accounts which it creates.

**List known addresses**

```
$ mockup-client list known addresses
```

**Transfer tokens**

```
$  mockup-client transfer 100 from bootstrap1 to bootstrap2
```

Let’s check that the transfer has been registered:

1. First, we check that the sender bootstrap1 has indeed paid the transfer amount, plus fees:

```
$ mockup-client get balance for bootstrap1
```

2. Second, we check that the receiver bootstrap2 indeed has an extra 100 tz in its balance:

```
$ mockup-client get balance for bootstrap2
```

**Something more advanced: interacting with contracts**





## Tune mockup parameters

For simplicity, mockup mode - like sandboxed mode - uses default values for wallet and protocol parameters. These default settings can be inspected and overridden to sui your needs.



## TO CONTINUE

https://blog.nomadic-labs.com/introducing-mockup-mode-for-tezos-client.html#mockup-capabilities

## Indentation

To remove ambiguities for human readers, the parser enforces some indentation rules.
    
For sequences:
- All expressions in a sequence must be aligned on the same column.
- An exception is made when consecutive expressions fit on the same line, as long as the first of them is correctly aligned.
- All expressions in a sequence must be indented to the right of the opening curly brace by at least one column.
- The closing curly brace cannot be on the left of the opening one.

For primitive applications:
- All arguments in an application must be aligned on the same column.
- An exception is made when consecutive arguments fit on the same line, as long as the first of them is correctly aligned.
- All arguments in a sequence must be indented to the right of the primitive name by at least one column.

## Command line

```
./tezos-client --mode mockup --base-dir /tmp/mockup create mockup
alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'
mockup-client list known addresses
```











