{-|
Module      : Time
Description : Helper for time related
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

-}

module HCOTP.Data.Time
  (
    waitfor
  )
where

import Control.Monad (when)
import Data.Time --(getCurrentTime, addUTCTime, UTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process


waitfor :: Double -> IO ()
waitfor secs = do
    now <- liftIO getCurrentTime
    let waitUntil = addUTCTime (realToFrac secs) now
    waitfor' waitUntil

waitfor' :: UTCTime -> IO ()
waitfor' tm = do
    now <- liftIO getCurrentTime
    when (now < tm) $ do
        liftIO $ threadDelay 200000 -- adjust resolution
        waitfor' tm

