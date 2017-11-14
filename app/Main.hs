{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment (getArgs)
import System.Random
import System.Console.CmdArgs
import HCOTP.Data.Params (Params(..))
import HCOTP.Network.Master (runMaster)
import HCOTP.Network.Worker (runWorker)
import HCOTP.Computation.SumProd (sumprod)


-- | command line descriptions
pmaster = Master {
              host = "localhost" &= groupname "Connection" &= help "address for listening"
            , port = "10011" &= help "port for listening"
            , send_for = 10 &= groupname "Specific" &= help "sending time in seconds"
            , wait_for = 5 &= help "grace time in seconds"
            , with_seed = 77 &= help "random number generator seed"
          }
          &= help "special parameters to control job submission"

pworker = Worker {
              host = "localhost" &= help "address for listening"
            , port = "10012" &= help "port for listening"
         }
         &= help "worker parameters to setup listening connection"

mode =
    cmdArgsMode $ modes [pmaster, pworker] &= help "Cluster Haskell submission program"
                                           &= program "chotpeval"
                                           &= summary "ClusterHaskell_OTP v1.0"
-- | entry point
main :: IO ()
main = do


  ps <- cmdArgsRun mode
  putStrLn $ show ps

  case ps of
     Worker{host=host, port=port} -> runWorker ps --host port
     Master{host=host, port=port} -> runMaster ps --host port
     _ -> putStrLn $ show mode

  --showSomeSumProd


-- | evaluate and print a sumproduct of 42 random numbers
showSomeSumProd = do
  setStdGen $ mkStdGen 42
  sumx <- sumprod 42
  putStrLn $ show $ sumx

