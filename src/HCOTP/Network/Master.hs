module HCOTP.Network.Master
  (
  runMaster
  )
where

import Data.Time (getCurrentTime, addUTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))

master :: Params -> Backend -> [NodeId] -> Process ()
master ps@Master {send_for=sf, wait_for=wf} backend ws = do
  if length ws > 1
  then do
       liftIO . putStrLn $ "Workers: " ++ show ws
       -- sleep
       liftIO . putStrLn $ "setup .."
       setupNodes ws ps
       liftIO . putStrLn $ "sending .."
       now <- liftIO getCurrentTime
       let sendUntil = addUTCTime (fromIntegral sf) now
           waitUntil = addUTCTime (fromIntegral (sf + wf)) now
       waitfor ws sendUntil
       liftIO . putStrLn $ "waiting .."
       waitfor ws waitUntil
       liftIO . putStrLn $ "finishing .."
       terminateAllSlaves backend
  else do
       liftIO . putStrLn $ "not enough workers!"


setupNodes ws Master {with_seed=srng} = do
  liftIO . putStrLn $ show $ zip ws (tail ws ++ [head ws])

waitfor ws tm = do
  now <- liftIO getCurrentTime
  if now >= tm
  then return ()
  else do
       liftIO $ threadDelay $ 1000000
       waitfor ws tm

runMaster :: Params -> IO ()
runMaster ps@Master {host=h, port=p} = do
      backend <- initializeBackend h p initRemoteTable
      startMaster backend (master ps backend)

