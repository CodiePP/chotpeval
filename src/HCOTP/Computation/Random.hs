module HCOTP.Computation.Random
  (
  get_random
  )
where

import System.Random


-- | get a random number in the interval (0,1] (by specification)
--
--   here: lower bounded by 1.0e-32
get_random :: IO Double
get_random =
  getStdRandom (randomR (0.0, 1.0)) >>= \r -> return $ max 1.0e-32 r

