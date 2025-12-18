-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ParallelListComp   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ViewPatterns       #-}

module Main where

import Data.Proxy(Proxy(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad.IO.Class (MonadIO(..))
import System.Environment(getArgs)

import Servant.API
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (client, runClientM, BaseUrl(..), mkClientEnv, Scheme(..))

import Choreography
import Choreography.Location
import System.Exit (exitSuccess)

a :: Proxy "a"
a = Proxy

b :: Proxy "b"
b = Proxy

c :: Proxy "c"
c = Proxy

type API = "send" :> Capture "from" LocTm :> ReqBody '[PlainText] String :> PostNoContent

httpSend :: Show a => LocTm -> Int -> a -> IO ()
httpSend sender targetPort a = liftIO $ do
    mgr <- newManager defaultManagerSettings
    res <- runClientM (client @API Proxy sender $ show a) $ mkClientEnv mgr $ BaseUrl Http "localhost" targetPort ""

    case res of
      Left err -> putStrLn $ "Error: " <> show err
      Right _  -> pure ()

    threadDelay 1000000

httpRecv :: Int -> IO (Maybe String)
httpRecv port = do
  ref <- newIORef Nothing

  recvThread <- forkIO do
    run port $ serve @API Proxy \_ msg -> do
      NoContent <$ liftIO (writeIORef ref $ Just msg)

  threadDelay 1000000
  killThread recvThread

  readIORef ref

-- "Frontrun" choreographic communication by sending data to the same port.
frontrun :: Choreo IO ()
frontrun = do
  locally a \_ -> do
    httpSend "a" 3001 False
    exitSuccess

  cond (a, wrap True) \case
    False -> pure ()
    True -> do
      (b, wrap 2) ~> c
      pure ()

-- Open an an "under-the-table" HTTP connecion between `a` and `b` to exfiltrate data from `c`.
collusion :: Choreo IO ()
collusion = do
  locally a \_ -> do
    v <- httpRecv 2999
    print v

  vb <- (c, wrap 2) ~> b

  locally b \unwrap -> do
    httpSend "b" 2999 $ unwrap vb

  pure ()

-- Helpers for running the choreographies

choreos :: [Choreo IO ()]
choreos = [frontrun, collusion]

-- Assigns different ports to each choreography so they can be run in parallel.
httpConfig :: Int -> [LocTm] -> HttpConfig
httpConfig choreo ls = mkHttpConfig [(l, ("localhost", port)) | l <- ls | port <- [3000 + 1000 * choreo..]]

main :: IO ()
main = do
  [read -> choreo, self] <- getArgs
  runChoreography (httpConfig choreo ["a", "b", "c"]) (choreos !! choreo) self
