{-# LANGUAGE DeriveDataTypeable #-}

module HCOTP.Data.Params
  (
  Params(..)
  )
where

import System.Console.CmdArgs


-- | parameters for master and workers
data Params = Master {
                host :: String,
                port :: String,
                send_for :: Int,
                wait_for :: Int,
                with_seed :: Int }
              | Worker {
                host :: String,
                port :: String }
            deriving (Data,Typeable,Show,Eq)


