{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment (getArgs)
import System.Random
import System.Console.CmdArgs
import HC.Network.Master (runMaster)
import HC.Network.Slave (runSlave)
import HC.Computation.SumProd (sumprod)


data Params = Master {
                host :: String,
                port :: Int,
                send_for :: Int,
                wait_for :: Int,
                with_seed :: Int }
            | Slave {
                host :: String,
                port :: Int }
            deriving (Data,Typeable,Show,Eq)


pmaster = Master {
              host = "localhost" &= groupname "Connection" &= help "address for listening"
            , port = 10011 &= help "port for listening"
            , send_for = 10 &= groupname "Specific" &= help "sending time in seconds"
            , wait_for = 5 &= help "grace time in seconds"
            , with_seed = 77 &= help "random number generator seed"
          }
          &= help "special parameters to control job submission"

pslave = Slave {
              host = "localhost" &= help "address for listening"
            , port = 10012 &= help "port for listening"
         }
         &= help "slave parameters to setup listening connection"

mode =
    cmdArgsMode $ modes [pmaster, pslave] &= help "Cluster Haskell submission program"
                                        &= program "chotpeval"
                                        &= summary "ClusterHaskell_OTP v1.0"
-- | entry point
main :: IO ()
main = do


  ps <- cmdArgsRun mode
  putStr $ show ps

  showSomeSumProd


-- | evaluate and print a sumproduct of 42 random numbers
showSomeSumProd = do
  setStdGen $ mkStdGen 42
  sumx <- sumprod 42
  putStrLn $ show $ sumx

