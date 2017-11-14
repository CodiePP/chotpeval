module HCOTP.Computation.SumProd
  (
  sumprod
  )
where

import System.Random


sumprod :: Int -> IO Double
sumprod 0 = return 0.0
sumprod n = do
  r <- randomRIO (0.0, 1.0)
  x <- sumprod (n - 1)
  return $ r * (fromIntegral n) + x



