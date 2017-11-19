{-|
Module      : Main
Description : main program for Cloud Haskell evaluation
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

This programm will dispatch a massive distributed computation
on a cluster of loosely coupled instances.
Cloud Haskell is used for messaging between nodes.

The cluster's architecture is a ring structure, where every node
is connected to one next, and the last one to the first.

A @Controller@ is the creator of the ring architecture and initiates
the computation.
The @Worker@s are running in their own processes and are found by
Cloud Haskell through automated discovery.
Importance has been put on
* correctness
* generality
then,
* speed

Some optimizations are necessary and possible to achieve a higher throughput
in the network.

-}

module Main
  (
    main
  )
where

import System.Environment (getArgs)
import System.Random
import System.Console.CmdArgs
import HCOTP.Data.Params (Params(..))
import HCOTP.Network.Controller (runController)
import HCOTP.Network.Worker (runWorker)


-- | command line descriptions
pctrl = Controller {
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
    cmdArgsMode $ modes [pctrl, pworker] &= help "Cluster Haskell submission program"
                                           &= program "chotpeval"
                                           &= summary "ClusterHaskell_OTP v1.0"
-- | entry point
main :: IO ()
main = do

  -- parse arguments
  ps <- cmdArgsRun mode
  putStrLn $ show ps

  -- run either controller or worker
  case ps of
     Worker{host=host, port=port} -> runWorker ps
     Controller{host=host, port=port} -> runController ps


