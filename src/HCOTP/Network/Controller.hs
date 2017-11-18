{-|
Module      : Controller
Description : The controller instance
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : POSIX

The @Controller@ sets up the network by connecting nodes.
Then, initiates the computation.

-}

module HCOTP.Network.Controller
  (
    runController
  )
where

import Control.Monad (forM_)
import Data.Time (getCurrentTime, addUTCTime, UTCTime)
--import Data.Binary
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))
import HCOTP.Data.Time (waitfor)
import HCOTP.Network.Worker (onWorker, myRemoteTable)


-- | program for the controller
controller :: Params -> Backend -> [NodeId] -> Process ()
controller ps@Controller {with_seed=srng, send_for=sf, wait_for=wf} backend ws = do
  if length ws > 1
  then do
       liftIO . putStrLn $ "Workers: " ++ show ws
       liftIO . putStrLn $ "setup .."
       setupNodes ws srng sf wf
       liftIO . putStrLn $ "waiting .."
      --  liftIO $ threadDelay 100000  -- tenth of a second
       now <- liftIO getCurrentTime
       let sendUntil = addUTCTime (fromIntegral sf) now
           waitUntil = addUTCTime (fromIntegral (sf + wf)) now
       liftIO $ waitfor waitUntil
       liftIO . putStrLn $ "finishing .."
       terminateAllSlaves backend
  else do
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

-- waitfor :: UTCTime -> Process ()
-- waitfor tm = do
--   now <- liftIO getCurrentTime
--   if now >= tm
--   then return ()
--   else do
--        liftIO $ threadDelay 1000000
--        waitfor tm


-- | entry point
runController :: Params -> IO ()
runController ps@Controller {host=h, port=p} = do
      backend <- initializeBackend h p myRemoteTable
      startMaster backend (controller ps backend)

