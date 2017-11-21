# chotpeval

Cloud Haskell / OTP evaluation

[Cloud Haskell](http://haskell-distributed.github.io/)


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

check a file with:
```
  stack exec liquid -- --notermination src/HCOTP/Network/Worker.hs 
```

for the random number generator, its output is restricted to be in the interval (0,1] (not including zero)
```
  stack exec liquid -- --notermination src/HCOTP/Computation/Random.hs 
```

in the controller we validate that at least two nodes are present when setting up the network
```
  stack exec liquid -- --notermination src/HCOTP/Network/Controller.hs 
```

