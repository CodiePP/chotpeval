{-|
Module      : Params
Description : Parameters to the program
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : GHC

The program accepts parameters on the command line for
the "Worker"s or the main "Controller".

-}

{-# LANGUAGE DeriveDataTypeable #-}

module HCOTP.Data.Params
  (
    Params(..)
  )
where

import System.Console.CmdArgs


-- | parameters for controller and workers
data Params = Controller {
                host :: String,       -- ^ hostname
                port :: String,       -- ^ port number or service name
                send_for :: Int,      -- ^ seconds
                wait_for :: Int,      -- ^ seconds
                with_seed :: Int      -- ^ seeding the random number generator
                }
              | Worker {
                host :: String,       -- ^ hostname
                port :: String        -- ^ port number or service name
                }
            deriving (Data,Typeable,Show,Eq)


