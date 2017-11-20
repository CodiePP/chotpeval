{-|
Module      : Controller
Description : The controller instance
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

The "Controller" sets up the network by connecting nodes, actually "Worker"s.
Then, initiates the computation.

-}

{-# OPTIONS_HADDOCK ignore-exports #-}

module HCOTP.Network.Controller
  (
    runController
  )
where

import Control.Monad (forM_)
import Data.Time (getCurrentTime, addUTCTime)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))
import HCOTP.Data.Time (waitfor)
import HCOTP.Network.Worker (onWorker, myRemoteTable)


-- | program for the controller
--   LiquidHaskell checks for totality.
controller :: Params -> Backend -> [NodeId] -> Process ()
controller Worker {} _ _ = return ()   -- not my business
controller Controller {with_seed=srng, send_for=sf, wait_for=wf} backend ws =
  if length ws > 1
  then do
       liftIO . putStrLn $ "Workers: " ++ show ws
       liftIO . putStrLn $ "setup .."
       setupNodes ws srng sf wf
       liftIO . putStrLn $ "waiting .."
       now <- liftIO getCurrentTime
       let waitUntil = addUTCTime (fromIntegral (sf + wf)) now
       liftIO $ waitfor waitUntil
       liftIO . putStrLn $ "finishing .."
       terminateAllSlaves backend
  else
       liftIO . putStrLn $ "not enough workers!"


setupNodes ws srng sf wf = do
  -- connect nodes: 1 -> 2, ..., k-1 -> k, k -> 1
  let connections = zip3 [1..] ws (tail ws ++ [head ws])
  let count = length connections
  --liftIO . putStrLn $ show $ connections
  forM_ connections $ \(idx, n1, n2) -> do
    liftIO . putStrLn $ show n2 ++ " -> " ++ show n1
    spawn n1 $ onWorker (count - idx, n2, srng, sf, wf)
    --spawn n1 $ $(mkClosure 'onWorker) (n2, srng, sf, wf)


-- | entry point.
--   LiquidHaskell checks for totality.
runController :: Params -> IO ()
runController ps@Controller {host=h, port=p} = do
  backend <- initializeBackend h p myRemoteTable
  startMaster backend (controller ps backend)
runController Worker{} = do
  liftIO $ print "was called with wrong type of parameters!"
