## 8-12

### Achievements


Run the command : 

```
./../../_build/default/src/bin_optimization/main.exe -option optimize scripts/int_int_contract.tz -r output/i_i_small.json
```


```
------Take this node------
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int -47 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 0.000000

The node iss : { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 894.000000

The node iss : { parameter int ; storage int ; code { CDR ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 894.000000

#########################
 CONCLUSION:

 number of well-typed sol: 3
 number of ill-typed sol: 0
 best distance from well-typed nodes : 0.000000
 best distance from ill-typed nodes : 2147483647.000000
#########################

```

FIND !!!!!!






## 7-31 

### Achievements 


With backup files in *bin_optimization_06*

Run the command : 

```
./../../_build/default/src/bin_optimization/main.exe -option optimize scripts/int_int_contract.tz -r output/Test.json
```

Get these close programs sometimes:
```
Haven the node { parameter int ;
  storage int ;
  code { DROP ; PUSH nat 188693936653 ; DROP ; PUSH int 0 ; NIL operation ; PAIR } } , distance is 10.000000

```

```
Haven the node { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } } , distance is 10.000000

```

And I set 10 as the maximum times of iteration

The node iss : { parameter int ;
  storage int ;
  code { NEG ; DROP ; DROP ; PUSH int 29631 ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { NEG ; DROP ; NIL operation ; PAIR } }
The node iss : { parameter int ; storage int ; code { SWAP ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 76 ; NIL operation ; PAIR } }
The node iss : { parameter int ; storage int ; code { NIL operation ; PAIR } }
The node iss : { parameter int ; storage int ; code { SWAP ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int -98 ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int -193 ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { PUSH nat 11979784 ; PUSH int 0 ; NEG ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 252 ; NIL operation ; PAIR } }
The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }



### Add more functions for add and sub

### Another example add1

./../../_build/default/src/bin_optimization/main.exe scripts/add1.tz -option sample -n 10 -o Add1.json -json

./../../_build/default/src/bin_optimization/main.exe -option optimize scripts/add1.tz -r output/Add1.json


## Add loop to update the corresponding node

  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

The node iss : { parameter int ;
  storage int ;
  code { DROP ; PUSH int 0 ; NIL operation ; PAIR } }
This node is well-typed, the corresponding distance is 10.000000

#########################
 CONCLUSION:

 number of well-typed sol: 51
 number of ill-typed sol: 0
 best distance from well-typed nodes : 10.000000
 best distance from ill-typed nodes : 2147483647.000000
#########################

