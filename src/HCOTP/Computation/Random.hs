{-# OPTIONS_HADDOCK ignore-exports #-}

module HCOTP.Computation.Random
       (
         get_random_k
       )
where

import System.Random


-- | get a random number in the interval (0,1] (by specification)
--
--   here: lower bound is 1.0e-32
{-
get_random :: IO Double
get_random =
  getStdRandom (randomR (0.0, 1.0)) >>= \r -> return $ max 1.0e-32 r
-}

-- | integer version of random number in the range [1, 1000]
{-@  get_random_k :: IO {v : Nat | v >= 1 && v <= 1000 } @-}
get_random_k :: IO Int
get_random_k =
  getStdRandom (randomR (0, 1000))
    >>= (\v -> return $ max 1 v)
    >>= (\w -> return $ min 1000 w)

