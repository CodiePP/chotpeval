{-# OPTIONS_HADDOCK ignore-exports #-}

module HCOTP.Computation.SumProd
       (
         sumprod
       )
where

import HCOTP.Computation.Random (get_random_k)


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
      --r <- get_random_k >>= (\v -> return ((fromIntegral v) / 1000.0) )
      r <- mkrandom
      sumprod' n (k+1) $ acc + r * (fromIntegral k)

-- | make a random number in the range (0,1]
mkrandom :: IO Double
mkrandom = do
  get_random_k >>= (\v -> return ((fromIntegral v) / 1000.0))

