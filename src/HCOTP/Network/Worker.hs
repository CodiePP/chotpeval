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

{-# OPTIONS_HADDOCK ignore-exports #-}
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
import Data.Time (getCurrentTime, addUTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))
import HCOTP.Data.Time (waitfor)
import HCOTP.Computation.Random (get_random_k)


data Msg = Msg {idx :: Int, rnd :: Double, nid :: NodeId}
         deriving (Show, Typeable, Binary, Generic)
data PrintOut = PrintOut
              deriving (Show, Typeable, Binary, Generic)
data StopSending = StopSending
                 deriving (Show, Typeable, Binary, Generic)

data State = State {lastidx :: Int, sump :: Double}
           deriving (Typeable)

instance Show State where
  show State {lastidx=lastidx, sump=sump} =
    "<" ++ (show lastidx) ++ ", " ++ (show sump) ++ ">"

startstate :: State
startstate = State 0 0.0

-- | debugging output - choose one
debug_out :: String -> Process ()
debug_out _ = return ()
--debug_out m = liftIO $ print m


-- | make a random number in the range (0,1]
mkrandom :: IO Double
mkrandom = do
  get_random_k >>= (\v -> return ((fromIntegral v) / 1000.0))

-- | send out a new message to the next node
sendMessage :: (Binary a, Typeable a) => a -> Process ()
sendMessage !msg =
  nsend "nnext" msg

-- | compute next state
newstate :: State -> Int -> Double -> State
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


-- | action defines our reaction to incoming messages
data Action = Ignore State
            | PassOnly Msg State
            | PassUpdate Msg State
            | SendNew Int State

-- | compute next action
computeAction :: Int -> State -> Msg -> Process Action
computeAction !nidx !state@State{lastidx=lastidx, sump=sump} !msg@(Msg i r node)
  | lastidx > i =  return $ Ignore state
  | lastidx == i =   do
        mypid <- getSelfPid
        nws <- if (processNodeId mypid == node)
               then do
                  -- increase clock and send new message
                  return $ SendNew (i+1) state
               else do
                  return $ PassOnly msg state
        return nws
  | lastidx <= i - 1 = do
        return $ PassUpdate msg state
  | otherwise = do
        liftIO $ print $ "unexpected id received: " ++ show i
        return $ Ignore state

-- | apply an action and compute next state
applyAction :: Action -> Process State
applyAction (Ignore s) = return s
applyAction (PassOnly m s) = do
  sendMessage m
  return s
applyAction (PassUpdate m@(Msg i r node) s) = do
  sendMessage m
  return $ newstate s i r
applyAction (SendNew j s) = do
  mypid <- getSelfPid
  r <- liftIO mkrandom
  sendMessage $ Msg j r (processNodeId mypid)
  return $ newstate s j r

-- | listening for messages and sending
listener :: Int -> State -> Process ()
listener !nidx !state = do
  receiveWait [
    match (\StopSending -> collector state )   -- pass state to collector
    ,
    match (\msg@(Msg {}) -> do
      newstate' <- (computeAction nidx state msg >>= applyAction)
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
       r <- liftIO mkrandom
       sendMessage $ Msg 1 r (processNodeId mypid)


remotable ['on_Worker]

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

onWorker args = $(mkClosure 'on_Worker) args


-- | startup entry point.
--   LiquidHaskell checks for totality.
runWorker :: Params -> IO ()
runWorker Worker {host=h, port=p} = do
  backend <- initializeBackend h p myRemoteTable
  startSlave backend
runWorker Controller{} = do
  liftIO $ print "was called with wrong type of parameters!"

