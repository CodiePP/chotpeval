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

--import Debug.Trace (trace)
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
import HCOTP.Computation.Random (get_random)


-- | work definition
data Msg = Msg {idx :: Int, rnd :: Double, pid :: ProcessId}
         deriving (Show, Typeable, Binary, Generic)

-- | messages that go over the wire
data Starting = Starting    -- ^ start signal
           deriving (Show, Typeable, Binary, Generic)
data PrintOut = PrintOut    -- ^ signal printing of result
              deriving (Show, Typeable, Binary, Generic)
data StopSending = StopSending  -- ^ stop sending new message, just collect
                 deriving (Show, Typeable, Binary, Generic)
data MsgList = Nil | Cons Msg MsgList  -- ^ we send a list of messages in each communication
             deriving (Show, Typeable, Binary, Generic)

-- | the internal state
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
mkrandom = get_random

-- | send out a new message to the next node
sendMessage :: (Show a, Binary a, Typeable a) => a -> Process ()
sendMessage !msg = --do
  nsend "nnext" msg

-- | compute new state
newstate :: State -> MsgList -> Process State
newstate state@State{lastidx=lastidx, sump=sump} ms = do
  (idx', sump') <- accum ms (lastidx, sump)
  return $ State idx' sump'
    where accum :: MsgList -> (Int, Double) -> Process (Int, Double)
          accum Nil (i0, p0) = return (i0, p0)
          accum (Cons (Msg i r _) Nil) (i0, p0) = calcst p0 i r
          accum (Cons (Msg i r _) mgs) (i0, p0) = calcst p0 i r >>= accum mgs
          calcst :: Double -> Int -> Double -> Process (Int, Double)
          calcst p0 i r =
            return (i, p0 + fromIntegral i * r)

nextMessage :: Int -> Process Msg
nextMessage idx = do
  mypid <- getSelfPid
  r <- liftIO mkrandom
  return $ Msg (idx+1) r mypid

-- | register the listener on the previous node in the ring
reglistener :: Int -> NodeId -> Process ()
reglistener nidx node = do
  newpid <- getSelfPid
  -- register this process with prev node
  registerRemoteAsync node "nnext" newpid
  reply <- expect :: Process RegisterReply
  liftIO $ putStrLn $ show reply
  -- start listener
  listener {-nidx-} startstate


-- | helper functions for MsgList
excludemine :: ProcessId -> MsgList -> MsgList
excludemine _ Nil = Nil
excludemine mypid (Cons m@(Msg _ _ pid) mgs)
            | pid == mypid = excludemine mypid mgs
            | otherwise = Cons m (excludemine mypid mgs)

append :: MsgList -> Msg -> MsgList
append Nil _ = Nil
append (Cons m0 Nil) m = Cons m0 (Cons m Nil)
append (Cons m0 ms) m = Cons m0 (append ms m)

-- | action defines our reaction to incoming messages
data Action = Ignore State
            | PassUpdate MsgList State
            | Start

-- | compute next action
computeAction :: State -> MsgList -> Process Action
computeAction state Nil = return $ Ignore state
computeAction state ms = do
  mypid <- getSelfPid
  let ms' = excludemine mypid ms
  return $ PassUpdate ms' state

-- | apply an action and compute next state
applyAction :: Action -> Process State
applyAction (Ignore s) = return s
applyAction Start = do
  m <- nextMessage 0
  sendMessage $ Cons m Nil
  newstate startstate (Cons m Nil)
applyAction (PassUpdate ms s) = do
  s'@(State idx sump) <- newstate s ms
  -- update the random number generator
  mapM_ (\_ -> liftIO mkrandom) [(1 + lastidx s) .. idx]
  m <- nextMessage idx
  sendMessage $ append ms m
  newstate s' (Cons m Nil)

-- | each "Worker" is in one of two states:
--   either as "listener" for incoming messages, processing them and sending it off to the next ode
--   and adding a new message to it.
--
--   or, as "collector" reacting to incoming messages, just processing them

-- | listening for messages and sending
listener :: State -> Process ()
listener state = do
  receiveWait [
    match (\StopSending -> do
      -- switching state from listener to collector
      debug_out $ "switching to collector!"
      sendMessage StopSending  -- pass message to next node
      collector state )   -- pass state to collector
    ,
    match (\msgs@(Cons _ _) -> do
      -- given message and state, compute and apply action, return new state
      state' <- (computeAction state msgs >>= applyAction)
      listener state' )
    ,
    match (\Starting -> do
      state' <- applyAction Start
      listener state' )
    ]

-- | listening for messages and collecting (no sending)
collector :: State -> Process ()
collector state@State{lastidx=lastidx, sump=sump} = do
  mypid <- getSelfPid
  receiveWait [
    match (\PrintOut -> do
        --debug_out $ "Finished with " ++ show state
        say $ "Finished with " ++ show state )
    ,
    match (\msgs@(Cons _ _) -> do
        --debug_out $ "collector " ++ (show msgs)
        let ms' = excludemine mypid msgs
        case ms' of
          Nil -> collector state
          _   -> do
                   sendMessage ms'
                   state' <- newstate state ms'
                   collector state'
        )
    ]


-- | closure accessible from remote
on_Worker :: (Int, NodeId, Int, Int, Int) -> Process ()
on_Worker (nidx, node, srng, sendsecs, gracesecs) = do
  --mypid <- getSelfPid

  -- set random number generator seed
  liftIO $ setStdGen $ mkStdGen srng

  --now <- liftIO getCurrentTime
  --let sendUntil = addUTCTime (fromIntegral sendsecs) now
  --    waitUntil = addUTCTime ((fromIntegral (sendsecs + gracesecs)) - 0.5) now
  let sendUntil = fromIntegral sendsecs
      waitUntil = fromIntegral (sendsecs + gracesecs) - 0.5

  -- start process, connect to next node
  newpid <- spawnLocal (reglistener nidx node)

  -- stop sending messages after this time
  _ <- spawnLocal (do
            liftIO $ do
              waitfor sendUntil
              now <- liftIO getCurrentTime
              putStrLn $ show now ++ " ... stopping ... " ++ show newpid
            send newpid StopSending
          )
  -- print out result before being killed
  _ <- spawnLocal (do
            liftIO $ do
              waitfor waitUntil
              now <- liftIO getCurrentTime
              putStrLn $ show now ++ " ... print ... " ++ show newpid
            send newpid PrintOut
          )

  -- receive and send messages
  liftIO $ threadDelay 100000  -- tenth of a second
  liftIO $ do
    now <- liftIO getCurrentTime
    putStrLn $ show now ++ " ... starting ... " ++ show newpid
  say $ "waiting seconds: " ++ show waitUntil

  -- mypi <- getProcessInfo mypid
  -- liftIO $ putStrLn $ show mypi

  -- start clock with last added node
  when (nidx == 0) $
    sendMessage Starting


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
runWorker Controller{} =
  liftIO $ print "was called with wrong type of parameters!"

