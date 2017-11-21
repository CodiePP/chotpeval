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
{-# LANGUAGE RankNTypes            #-}

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
controller Controller {} _ [] = do
       liftIO . putStrLn $ "no workers!"
       return ()
controller Controller {} _ [_] = do
       liftIO . putStrLn $ "not enough workers!"
       return ()
controller Controller {with_seed=srng, send_for=sf, wait_for=wf} backend ws@(_:_) = do
       liftIO . putStrLn $ "Workers: " ++ show ws
       liftIO . putStrLn $ "setup .."
       setupNodes ws srng sf wf
       liftIO . putStrLn $ "waiting .."
       now <- liftIO getCurrentTime
       let waitUntil = addUTCTime (fromIntegral (sf + wf)) now
       liftIO $ waitfor waitUntil
       liftIO . putStrLn $ "finishing .."
       terminateAllSlaves backend


-- | enforce that at least two nodes are present when setting up the network
{-@ setupNodes :: {ns:[NodeId] | len ns >= 2 } -> Int -> Int -> Int -> Process () @-}
setupNodes :: [NodeId] -> Int -> Int -> Int -> Process ()
setupNodes ws srng sf wf = do
  -- connect nodes: 1 -> 2, ..., k-1 -> k, k -> 1
  let connections' = connections ws
      count = length connections'
  --liftIO . putStrLn $ show $ connections
  forM_ connections' $ \(idx, n1, n2) -> do
    liftIO . putStrLn $ show n2 ++ " -> " ++ show n1
    spawn n1 $ onWorker (count - idx, n2, srng, sf, wf)

-- | assuming the length of input lists >= 2, that the length of the output is equal
--
--   LH did not infer on "zip3".
{-@ assume zip3 :: forall a b c . ls1:[a] -> ls2:[b] -> ls3:[c] -> {ts:[(a,b,c)] | len ts == len ls1 && len ts == len ls2 && len ts == len ls3} @-}
{-@ nzip3 :: forall a b . {ns1:[a] | len ns1 >= 2} -> {ns2:[b] | len ns2 == len ns1} -> {ts:[(Int,a,b)] | len ts == len ns1} @-}
nzip3 :: forall a b . [a] -> [b] -> [(Int, a, b)]
nzip3 as bs = zip3 [1..] as bs

-- | enforce that input list contains at least two nodes
--
--   and output list has the same size
{-@ connections :: {ns:[NodeId] | len ns >= 2} -> ts:{[(Int,NodeId,NodeId)]| len ts >= 2} @-}
connections :: [NodeId] -> [(Int, NodeId, NodeId)]
connections ws =
  nzip3 ws (tail ws ++ [head ws])


-- | entry point.
--   LiquidHaskell checks for totality.
runController :: Params -> IO ()
runController ps@Controller {host=h, port=p} = do
  backend <- initializeBackend h p myRemoteTable
  startMaster backend (controller ps backend)
runController Worker{} = do
  liftIO $ print "was called with wrong type of parameters!"
