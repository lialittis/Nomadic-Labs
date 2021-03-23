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


