module HC.Network.Master
  (
  runMaster
  )
where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

runMaster host port = do
  backend <- initializeBackend host port initRemoteTable
  startMaster backend (master backend)

