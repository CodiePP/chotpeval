{-|
Module      : Worker
Description : The worker instance
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

The @Worker@ is being called by the @Controller@.
Every @Worker@ has a next instance to which it will send messages to.
This leads to a ring architecture.

-}

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

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
import Control.Monad (when)
import System.Random
import Data.Time (getCurrentTime, addUTCTime, UTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))
import HCOTP.Data.Time (waitfor)
import HCOTP.Computation.Random (get_random)


data Msg = Msg {idx :: Int, rnd :: Double, nid :: NodeId}
         deriving (Show, Typeable, Binary, Generic)
data PrintOut = PrintOut
              deriving (Show, Typeable, Binary, Generic)
data StopSending = StopSending
                 deriving (Show, Typeable, Binary, Generic)

data State = State {lastidx :: Int, sump :: Double}
           deriving (Show, Typeable)

startstate = State 0 0.0

-- | debugging output - choose one
debug_out _ = return ()
--debug_out m = liftIO $ print m

-- | send out a new message to the next node
sendMessage :: (Binary a, Typeable a) => a -> Process ()
sendMessage !msg =
  nsend "nnext" msg

-- | compute next state
newstate !state@State{lastidx=lastidx, sump=sump} !i !r =
  if (lastidx == i - 1)
     then State i (sump + fromIntegral i * r)
     else state

-- | register the listener on the previous node in the ring
reglistener :: Int -> NodeId -> Process ()
reglistener nidx node = do
  newpid <- getSelfPid
  -- register this process with prev node
  registerRemoteAsync node "nnext" newpid
  reply <- expect :: Process RegisterReply
  --liftIO $ putStrLn $ show reply
  -- start listener
  listener nidx startstate


msghandler !nidx !state@State{lastidx=lastidx, sump=sump} !msg@(Msg i r node)
  | lastidx > i =  return state   -- ignore
  | lastidx == i =   do
        mypid <- getSelfPid
        nws <- if (processNodeId mypid == node)
               then do
                  -- increase clock and send new message
                  r' <- liftIO get_random
                  sendMessage $ Msg (i+1) r' node
                  return $ newstate state (i+1) r'
               else do
                  sendMessage msg -- pass to next node
                  return state
        return nws
  | lastidx <= i - 1 = do
        sendMessage msg -- pass to next node
        return $ newstate state i r
  | otherwise = do
        liftIO $ print $ "unexpected id received: " ++ show i
        return state

-- | listening for messages and sending
listener :: Int -> State -> Process ()
listener !nidx !state = do
  mypid <- getSelfPid
  receiveWait [
    match (\StopSending -> collector state )   -- ^ pass state to collector
    ,
    match (\msg@(Msg i r node) -> do
      newstate' <- msghandler nidx state msg
      listener nidx newstate' )
    ]

-- | listening for messages and collecting (no sending)
collector :: State -> Process ()
collector !state@State{lastidx=lastidx, sump=sump} = do
  mypid <- getSelfPid
  receiveWait [
    match (\PrintOut -> do
        debug_out $ "Finished with " ++ show state
        say $ "Finished with " ++ show state )
    ,
    match (\msg@(Msg i r node) -> do
        -- say $ "received #" ++ (show i) ++ " from " ++ (show node)
        --when (i > lastidx + 1) $ say $ "unexpected index in message: " ++ show i
        when (node /= processNodeId mypid) $ sendMessage msg   -- pass to next node
        debug_out $ "collector " ++ (show i) ++ " " ++ (show $ newstate state i r)
        collector $ newstate state i r )
    ]


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
  newpid <- spawnLocal (reglistener idx node)

  -- stop sending messages after this time
  _ <- spawnLocal (do
            liftIO $ do
              waitfor sendUntil
              putStrLn $ "... stopping ..." ++ show newpid
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
  say $ "waiting until " ++ show waitUntil

  -- mypi <- getProcessInfo mypid
  -- liftIO $ putStrLn $ show mypi

  -- start clock with last added node
  when (idx == 0) $ do
       r <- liftIO get_random
       sendMessage $ Msg 1 r (processNodeId mypid)


remotable ['on_Worker]

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

onWorker args = $(mkClosure 'on_Worker) args


-- | startup entry point
runWorker :: Params -> IO ()
runWorker ps@Worker {host=h, port=p} = do
  backend <- initializeBackend h p myRemoteTable
  startSlave backend

