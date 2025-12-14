{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ParallelListComp   #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Benchmark where

import Control.Concurrent
import Control.Monad(when)
import Data.Foldable(for_)
import Data.IORef
import Data.Proxy(Proxy(..))
import Data.Time
import Data.Traversable(for)
import GHC.TypeLits(KnownSymbol)

import Choreography hiding (cond, cond', cond_, cond_')
import Choreography qualified as C
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Local
import Choreography.Network.LocalMsgCount

-- Benchmarking infrastructure

-- The type of the `cond_'` function, to avoid writing it out multiple times.
-- NB: This is a more specific version of the original `cond'` function's type.
type Cond_' =
  forall m l a b
  . (Bounded a, Enum a, Show a, Read a, KnownSymbol l)
  => (Proxy l, Unwrap l -> m a)
  -> (a -> Choreo m (b @ l))
  -> Choreo m (b @ l)

httpConfig :: [LocTm] -> HttpConfig
httpConfig ls = mkHttpConfig [(l, ("localhost", port)) | l <- ls | port <- [3000..]]

data SomeBackend = forall b. Backend b => SomeBackend b

runWithBackend :: SomeBackend -> Choreo IO () -> LocTm -> IO ()
runWithBackend (SomeBackend b) = runChoreography b

mkBackendConfig :: String -> IORef Int -> [LocTm] -> IO SomeBackend
mkBackendConfig "http"  _ = pure . SomeBackend . httpConfig
mkBackendConfig "local" _ = fmap SomeBackend . mkLocalConfig
mkBackendConfig "count" msgCounter = fmap SomeBackend . mkLocalMsgCountConfig msgCounter
mkBackendConfig _ _ = error "Invalid backend"

timed :: IO () -> IO ()
timed io = do
  before <- getCurrentTime
  io
  after <- getCurrentTime
  print $ diffUTCTime after before

-- Run a choreography with both the original `cond'` and the non-broadcasting `cond_'` and report their execution time.
benchmarkChoreography :: String -> String -> [LocTm] -> (Cond_' -> Choreo IO ()) -> IO ()
benchmarkChoreography backend cond locs choreo = do
  let chor = choreo $ if cond == "full" then C.cond' else C.cond_'
  msgCounter <- newIORef 0
  someCfg <- mkBackendConfig backend msgCounter locs

  timed do
    mvars <- for locs \l -> do
      mvar <- newEmptyMVar

      forkFinally (runWithBackend someCfg chor l) \_ -> putMVar mvar ()
      pure mvar

    for_ mvars takeMVar

  when (backend == "count") do
    count <- readIORef msgCounter
    putStrLn $ "Sends: " <> show count
