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
import Data.Time (getCurrentTime, UTCTime)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process


waitfor :: UTCTime -> IO ()
waitfor tm = do
    now <- liftIO getCurrentTime
    when (now < tm) $ do
        liftIO $ threadDelay 100000 -- adjust resolution
        waitfor tm

