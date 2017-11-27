# chotpeval

Cloud Haskell / OTP evaluation


This program uses [Cloud Haskell](http://haskell-distributed.github.io/) to dispatch jobs to nodes on the network and collect the result.

The nodes are connected in a ring structure. Every node has one neighbour it can communicate with. 
This leads to *n* network connections with *n* nodes.

A message is passed from one node to another, until it reaches the originator again. On message receipt every node does its job and attaches a new message to the incoming messages, then sends these to its next node. Therefore, a package contains at most n messages when received and resent in a ring with n nodes.

## Running

```
./bin/chotpeval worker -p 10010 &
./bin/chotpeval worker -p 10011 &
./bin/chotpeval worker -p 10012 &
./bin/chotpeval controller --send-for=10 --wait-for=5 --with-seed=77
```
On my dated laptop this has a throughput of about 8000 messages per second.

## Stack

* stack build; stack install
* stack haddock
* stack test

## Verification

in GHCi the distributed computation can be verified:

```
stack ghci
ghci> setStdGen $ mkStdGen <rng seed>
ghci> sumprod <lastindex>
```

## Liquid Haskell

Using [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell) types in Haskell can be restrained further and the propositions automatically checked with a SMT solver (e.g. [z3](https://github.com/Z3Prover/z3)).

for the random number generator, its output is restricted to be in the interval (0,1] (not including zero)
```
  stack exec liquid -- --notermination src/HCOTP/Computation/Random.hs 
```

in the controller we validate that at least two nodes are present when running the network
```
  stack exec liquid -- --notermination src/HCOTP/Network/Controller.hs 
```

