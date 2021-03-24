# Mockup mode

## Run a mokeup client in stateless mode

**Typecheck and evaluate scripts**

* Typecheck
`
$ tezos-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
  --mode mockup typecheck script hardlimit.tz
`

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

Note that, without --protocol option, the mockup mode will choose a protocol for you.


**Query available mockup protocols**

We can query the list of the Tezos protocols that mockup mode supports:

`
$ tezos-client list mockup protocols
`

A word on the protocol for naming protocols: The Tezos protocol IDs above are based on hashes, but the start of each ID hints at the release name of the corresponding protocol. The three items above correspond to protocols called alpha (a development version of the Tezos protocol), Delphi, and Carthage.

Getting these IDs matters because a Tezos blockchain requires a protocol, thus in particular setting up a mockup state requires us to choose a protocol. The list above tells us what’s available.



## Run a mockup client with state

Giving the mockup client some state allows access more of the available functionalities. In particular, given a state we can operate on it, including:

- transferring Tez cryptocurrency tokens (ꜩ),
- originating (deploying) contracts,
- importing keys, and
- querying balances or (more generally) making RPC queries on the chain’s current state.


### A useful command alias: mockup-client

A shell alias will let us call tezos-client with --mode mockup and --base-dir /tmp/mockup, and so save us keystrokes later:

`
$ alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'
`

## TO CONTINUE

https://blog.nomadic-labs.com/introducing-mockup-mode-for-tezos-client.html#the-basic-command---tezos-client-




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











