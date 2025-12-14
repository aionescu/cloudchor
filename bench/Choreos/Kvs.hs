{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This is a version of the `kvs-2-primary-backup` choreography, adapted for the benchmarking setup.
module Choreos.Kvs where

import Data.Foldable(for_)
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy

import Benchmark
import Choreography hiding (cond, cond_, cond', cond_')
import Choreography.Location(wrap)

client :: Proxy "client"
client = Proxy

primary :: Proxy "primary"
primary = Proxy

backup :: Proxy "backup"
backup = Proxy

type State = Map String String

data Request = Put String String | Get String deriving stock (Show, Read)

isPutRequest :: Request -> Bool
isPutRequest (Put _ _) = True
isPutRequest _ = False

requests :: [Request]
requests =
  [ Put "x" "a"
  , Put "y" "b"
  , Get "x"
  , Put "y" "c"
  , Get "y"
  ]

type Response = Maybe String

-- | `handleRequest` handle a request and returns the new the state.
handleRequest :: Request -> IORef State -> IO Response
handleRequest request stateRef = case request of
  Put key value -> do
    modifyIORef stateRef (Map.insert key value)
    return (Just value)
  Get key -> do
    state <- readIORef stateRef
    return (Map.lookup key state)

-- | `kvs` is a choreography that processes a single request located at the client and returns the response.
-- If the request is a `PUT`, it will forward the request to the backup node.
kvs ::
  Cond_' ->
  Request @ "client" ->
  IORef State @ "primary" ->
  IORef State @ "backup" ->
  Choreo IO (Response @ "client")
kvs cond_' request primaryStateRef backupStateRef = do
  -- send request to the primary node
  request' <- (client, request) ~> primary

  -- branch on the request
  cond_' (primary, \unwrap -> pure $ isPutRequest $ unwrap request') \case
    -- if the request is a `PUT`, forward the request to the backup node
    True -> do
      request'' <- (primary, request') ~> backup
      ack <-
        backup `locally` \unwrap -> do
          handleRequest (unwrap request'') (unwrap backupStateRef)
      (backup, ack) ~> primary
      return $ wrap ()
    _ -> do
      return $ wrap ()

  -- process request on the primary node
  response <-
    primary `locally` \unwrap ->
      handleRequest (unwrap request') (unwrap primaryStateRef)

  -- send response to client
  (primary, response) ~> client

kvs' :: Cond_' -> Choreo IO ()
kvs' cond_' = do
  primaryStateRef <- primary `locally` \_ -> newIORef (Map.empty :: State)
  backupStateRef <- backup `locally` \_ -> newIORef (Map.empty :: State)
  for_ requests \request -> do
    -- Replaced the interactive prompt with a dummy `locally` to keep the choreography's structure,
    -- for testing the overhead of `cond_`'s static analysis.
    clientRequest <- client `locally` \_ -> pure request
    kvs cond_' clientRequest primaryStateRef backupStateRef
    client `locally` \_ -> pure () -- putStrLn ("> " ++ show (unwrap response)) -- Don't print during benchmarking
    pure ()
