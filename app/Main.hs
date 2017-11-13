module Main where

import System.Environment (getArgs)
import System.Random
import HC.Network.Master (runMaster)
import HC.Network.Slave (runSlave)
import HC.Computation.SumProd (sumprod)


-- | entry point
main :: IO ()
main = do

  args <- getArgs
  case args of
    "master" : host : port : [] -> do
      runMaster host port
    "slave" : host : port : [] -> do
      runSlave host port
    _ -> do
      putStrLn "sorry, that was wrong! Please, follow the rules."

  showSomeSumProd


-- | evaluate and print a sumproduct of 42 random numbers
showSomeSumProd = do
  setStdGen $ mkStdGen 42
  sumx <- sumprod 42
  putStrLn $ show $ sumx

