-- SPDX-FileCopyrightText: 2022 Gan Shen
-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

-- | This module defines the multi-thread backend for the `Network` monad.
module Choreography.Network.Local where

import Choreography.Location
import Choreography.Network
import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap

-- | Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

newtype LocalConfig = LocalConfig
  { locToBuf :: HashMap LocTm MsgBuf
  }

newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      return (HashMap.insert loc chan hash)

mkLocalConfig :: [LocTm] -> IO LocalConfig
mkLocalConfig locs = LocalConfig <$> foldM f HashMap.empty locs
  where
    f hash loc = do
      buf <- newEmptyMsgBuf locs
      return (HashMap.insert loc buf hash)

locsLocal :: LocalConfig -> [LocTm]
locsLocal = HashMap.keys . locToBuf

runNetworkLocal :: MonadIO m => LocalConfig -> LocTm -> Network m a -> m a
runNetworkLocal cfg self prog = interpFreer handler prog
  where
    handler :: MonadIO m => NetworkSig m a -> m a
    handler (Run m)      = m
    handler (Send a l)   = liftIO $ writeChan ((locToBuf cfg ! l) ! self) (show a)
    handler (Recv l)     = liftIO $ read <$> readChan ((locToBuf cfg ! self) ! l)
    handler (BCast a ls) = mapM_ handler $ fmap (Send a) ls

instance Backend LocalConfig where
  locs = locsLocal
  runNetwork = runNetworkLocal
