-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE GADTs #-}

-- | This module defines an instrumented version of the `Local` backend,
-- which counts how many `Send` operations are performed during execution of the choreography.
-- It is used by the benchmarks in `benchmark/`.
module Choreography.Network.LocalMsgCount where

import Choreography.Location
import Choreography.Network
import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.IORef

-- | Each location is associated with a message buffer which stores messages sent
-- from other locations.
type MsgBuf = HashMap LocTm (Chan String)

data LocalMsgCountConfig = LocalMsgCountConfig
  { msgCounter :: IORef Int
  , locToBuf :: HashMap LocTm MsgBuf
  }

newEmptyMsgBuf :: [LocTm] -> IO MsgBuf
newEmptyMsgBuf = foldM f HashMap.empty
  where
    f hash loc = do
      chan <- newChan
      return (HashMap.insert loc chan hash)

mkLocalMsgCountConfig :: IORef Int -> [LocTm] -> IO LocalMsgCountConfig
mkLocalMsgCountConfig msgCounter locs = LocalMsgCountConfig msgCounter <$> foldM f HashMap.empty locs
  where
    f hash loc = do
      buf <- newEmptyMsgBuf locs
      return (HashMap.insert loc buf hash)

locsLocalMsgCount :: LocalMsgCountConfig -> [LocTm]
locsLocalMsgCount = HashMap.keys . locToBuf

runNetworkLocalMsgCount :: MonadIO m => LocalMsgCountConfig -> LocTm -> Network m a -> m a
runNetworkLocalMsgCount cfg self prog = interpFreer handler prog
  where
    handler :: MonadIO m => NetworkSig m a -> m a
    handler (Run m)      = m
    handler (Send a l)   = liftIO $ do
      atomicModifyIORef' (msgCounter cfg) (\c -> (c + 1, ()))
      writeChan ((locToBuf cfg ! l) ! self) (show a)
    handler (Recv l)     = liftIO $ read <$> readChan ((locToBuf cfg ! self) ! l)
    handler (BCast a ls) = mapM_ handler $ fmap (Send a) ls

instance Backend LocalMsgCountConfig where
  locs = locsLocalMsgCount
  runNetwork = runNetworkLocalMsgCount
