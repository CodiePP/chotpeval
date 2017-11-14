module HCOTP.Network.Worker
  (
  runWorker
  )
where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import HCOTP.Data.Params (Params(..))


runWorker ps@Worker {host=h, port=p} = do
  backend <- initializeBackend h p initRemoteTable
  startSlave backend

