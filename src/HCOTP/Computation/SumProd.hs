{-|
Module      : SumProd
Description : calculates sum of products
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

-}

{-# OPTIONS_HADDOCK ignore-exports #-}

module HCOTP.Computation.SumProd
       (
         sumprod
       )
where

import HCOTP.Computation.Random (get_random)


-- | iteratively generate random numbers and multiply them with their index.
--   Then, sum it all up.
--   This method only exists so one can verify the score in the distributed computation
sumprod :: Int -> IO Double
sumprod n
  | n == 0     = return 0.0
  | otherwise = sumprod' (n + 1) 1 0.0

sumprod' n k acc
  | n == k     =  return acc
  | otherwise = do
      r <- get_random
      sumprod' n (k+1) $ acc + r * (fromIntegral k)

