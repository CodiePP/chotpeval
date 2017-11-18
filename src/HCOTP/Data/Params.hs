{-|
Module      : Params
Description : Parameters to the program
Copyright   : (c) 2017 Alexander Diemand
License     : BSD-3
Maintainer  : codieplusplus@apax.net
Stability   : experimental
Portability : POSIX

The program accepts parameters on the command line for
the @Worker@s or the main @Controller@.

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
                host :: String,
                port :: String,
                send_for :: Int,
                wait_for :: Int,
                with_seed :: Int }
              | Worker {
                host :: String,
                port :: String }
            deriving (Data,Typeable,Show,Eq)


