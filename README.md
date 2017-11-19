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

