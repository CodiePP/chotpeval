module HCOTP.Computation.SumProd
  (
  sumprod
  )
where

import HCOTP.Computation.Random (get_random)


sumprod :: Int -> IO Double
sumprod n
  | n == 0     = return 0.0
  | otherwise = sumprod' (n + 1) 1 0.0

sumprod' n k acc
  | n == k     =  return acc
  | otherwise = do
      r <- get_random
      sumprod' n (k+1) $ acc + r * (fromIntegral k)



