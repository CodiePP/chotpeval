module HC.Network.Slave
  (
  runSlave
  )
where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

runSlave host port = do
  backend <- initializeBackend host port initRemoteTable
  startSlave backend

