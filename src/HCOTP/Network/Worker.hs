{-|
Module      : Worker
Description : The worker instance
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : POSIX

The @Worker@ is being called by the @Controller@.
Every @Worker@ has a next instance to which it will send messages to.
This leads to a ring architecture.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module HCOTP.Network.Worker
  (
    runWorker
  , onWorker
  , myRemoteTable
  )
where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Random
import Data.Time (getCurrentTime, addUTCTime, UTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))
import HCOTP.Data.Time (waitfor)


data Msg = Msg {idx :: Int, rnd :: Double, nid :: NodeId}
         deriving (Show, Typeable, Binary, Generic)
data PrintOut = PrintOut
              deriving (Show, Typeable, Binary, Generic)
data StopSending = StopSending
                 deriving (Show, Typeable, Binary, Generic)

data State = State {sending :: Bool, lastidx :: Int, sump :: Double}
           deriving (Show, Typeable)

startstate = State True 0 0.0

-- | send out a new message to the next node
send_message :: (Binary a, Typeable a) => a -> Process ()
send_message !msg = do
  nsend "nnext" msg

-- | register the listener on the previous node in the ring
reglistener :: NodeId -> Process ()
reglistener node = do
  newpid <- getSelfPid
  -- register this process with prev node
  registerRemoteAsync node "nnext" newpid
  reply <- expect :: Process RegisterReply
  liftIO $ putStrLn $ show reply
  -- start listener
  listener startstate

-- | listening for messages and reacting
listener :: State -> Process ()
listener state@State{sending=sending, lastidx=lastidx, sump=sump} = do
  mypid <- getSelfPid
  -- liftIO $ putStrLn $ "listener pid = " ++ (show mypid)
  receiveWait [
    match (\(PrintOut) -> do {say $ "Finished with " ++ (show state)} )
    ,
    match (\(StopSending) -> listener (State False lastidx sump) )
    ,
    match (\msg@(Msg i r node) -> do
        -- say $ "received #" ++ (show i) ++ " from " ++ (show node)
        if i > lastidx
          then send_message msg
          else return ()
        if sending && node == processNodeId mypid
          then do
             r' <- liftIO get_random
             send_message $ Msg (i+1) r' node
          else return ()
        listener (State sending i ((fromIntegral i) * r)) )
    ]


-- | get a random number in the interval (0,1] (by specification)
--
--   here: lower bounded by 1.0e-32
--   TODO: make liquid constraint
get_random :: IO Double
get_random =
  getStdRandom (randomR (0.0, 1.0)) >>= \r -> return $ max (1.0e-32) r


-- | closure accessible from remote
on_Worker :: (Int, NodeId, Int, Int, Int) -> Process ()
on_Worker (idx, node, srng, sendsecs, gracesecs) = do
  mypid <- getSelfPid

  -- set random number generator seed
  liftIO $ setStdGen $ mkStdGen srng

  now <- liftIO getCurrentTime
  let sendUntil = addUTCTime (fromIntegral sendsecs) now
      waitUntil = addUTCTime (fromIntegral (sendsecs + gracesecs) - 0.3) now

  -- start process, connect to next node
  newpid <- spawnLocal (reglistener node)

  -- stop sending messages after this time
  _ <- spawnLocal (do
            liftIO $ do
              waitfor sendUntil
              putStrLn $ "... print ..." ++ show newpid
            send newpid StopSending
          )
  -- print out result before being killed
  _ <- spawnLocal (do
            liftIO $ do
              waitfor waitUntil
              putStrLn $ "... print ..." ++ show newpid
            send newpid PrintOut
          )

  -- receive and send messages
  liftIO $ threadDelay 100000  -- tenth of a second
  say $ "waiting until " ++ (show waitUntil)

  -- mypi <- getProcessInfo mypid
  -- liftIO $ putStrLn $ show mypi

  -- start clock with last added node
  if idx == 0
    then do
       r <- liftIO get_random
       send_message $ Msg 1 r (processNodeId mypid)
    else return ()


remotable ['on_Worker]

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

onWorker args = $(mkClosure 'on_Worker) args


-- | startup entry point
runWorker ps@Worker {host=h, port=p} = do
  backend <- initializeBackend h p myRemoteTable
  startSlave backend

