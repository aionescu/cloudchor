-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}

-- This is a version of the `clean-room` choreography, adapted for the benchmarking setup.
module Choreos.CleanRoom where

import Data.Function((&))
import Data.Proxy(Proxy(..))
import Data.SOP.BasicFunctors(mapKK, K (..))
import Data.SOP.Classes(HCollapse(..), hmap, hsequenceK)
import Data.SOP.Constraint(All)
import Data.SOP.NP(NP(..))
import GHC.TypeLits(KnownSymbol, SSymbol, withKnownSymbol, withSomeSSymbol)

import Benchmark
import Choreography hiding (cond, cond_, cond', cond_')
import Choreography.Location
import Choreography.Location.Multi

-- Schema and dataset handling (stubbed out)

data Schema = Schema
  deriving stock (Show, Read)

data Dataset = Dataset
  deriving stock (Show, Read)

data Query = Query
  deriving stock (Show, Read)

loadSchema :: KnownSymbol l => Proxy l -> IO Schema
loadSchema _ = pure Schema

mergeSchemas :: [Schema] -> Schema
mergeSchemas _ = Schema

conformsToSchema :: Schema -> Dataset -> IO Bool
conformsToSchema _ _ = pure True

emptyDataset :: Dataset
emptyDataset = Dataset

-- Simulate some I/O to load a dataset (e.g. from a file)
loadDataset :: KnownSymbol l => Proxy l -> IO Dataset
loadDataset _ = pure Dataset

mergeDatasets :: [Dataset] -> Dataset
mergeDatasets _ = Dataset

loadQuery :: KnownSymbol l => Proxy l -> Schema -> IO Query
loadQuery _ _ = pure Query

agreeWithQueries :: KnownSymbol l => Proxy l -> [Query] -> IO Bool
agreeWithQueries _ _ = pure True

runQuery :: Dataset -> Query -> IO Int
runQuery _ _ = pure 0

-- Error-handling (if datasets don't conform to schema)

getClientDataset :: (KnownSymbol server, KnownSymbol client) => Cond_' -> Int -> Int -> Schema @ server -> Proxy server -> Proxy client -> Choreo IO (Dataset @ server)
getClientDataset cond_' failures tries schema server client =
  if tries <= 0 then
    pure $ wrap emptyDataset
  else do
    dataset <- (client, \_ -> loadDataset client) ~~> server
    cond_'
      (server, \unwrap ->
        if failures > 0 then
          pure False -- Simulate a fixed number of failures for each client
        else
          conformsToSchema (unwrap schema) (unwrap dataset)
      )
      \case
        True -> pure dataset
        False -> getClientDataset cond_' (failures - 1) (tries - 1) schema server client

getClientDatasets :: (KnownSymbol server, All KnownSymbol clients) => Cond_' -> Int -> Int -> Schema @ server -> Proxy server -> NP Proxy clients -> Choreo IO (NP (K Dataset) clients @ server)
getClientDatasets _ _ _ _ _ Nil = pure $ wrap Nil
getClientDatasets cond_' failures tries schema server (client :* clients) = do
  dataset <- getClientDataset cond_' failures tries schema server client
  datasets <- getClientDatasets cond_' failures tries schema server clients
  locally server \unwrap -> pure $ K (unwrap dataset) :* unwrap datasets

-- Main choreography

cleanRoom :: (KnownSymbol server, All KnownSymbol clients) => Cond_' -> Int -> Proxy server -> NP Proxy clients -> Choreo IO ()
cleanRoom cond_' failures server clients = do
  -- Every client loads their data schema and sends it to the server
  schemas <- (clients, \c _ -> loadSchema c) *~~> server

  -- Server merges the schemas
  finalSchemaS <- locally server \unwrap -> pure $ mergeSchemas $ hcollapse $ unwrap schemas

  -- Server broadcasts the final schema
  finalSchema <- (server, finalSchemaS) ~>* clients

  -- Based on the final schema, each client computes what query it wants to run, and sends it back to the server
  queriesS <- (clients, \c unwrap -> loadQuery c $ unwrap finalSchema) *~~> server

  -- Server broadcasts the list of queries to all clients
  queries <- (server, \unwrap -> pure $ hcollapse $ unwrap queriesS) ~~>* clients

  -- Each client locally decides if they agree with everyone else's queries, and sends back a response
  agreements <- (clients, \c unwrap -> agreeWithQueries c $ unwrap queries) *~~> server

  -- Server branches on the responses
  cond_' (server, \unwrap -> pure $ and $ hcollapse $ unwrap agreements) \case
    False -> do -- Someone disagrees, end the protocol
      multilocally clients \_ _ -> pure () -- print "Disagreement"
      pure $ wrap ()
    True -> do -- Everyone agrees, proceed
      -- The amount of times the server will ask for a valid dataset
      let tries = 5

      -- Clients send their datasets to the server
      datasets <- getClientDatasets cond_' failures tries finalSchemaS server clients

      -- Server locally merges the datasets and runs all the queries
      resultsS <- locally server \unwrap -> do
        let aggregatedDataset = mergeDatasets $ hcollapse $ unwrap datasets
        hsequenceK $ hmap (mapKK $ runQuery aggregatedDataset) $ unwrap queriesS

      -- Server sends the result of each query "pointwise"
      results <- (server, resultsS) ~>. clients

      -- Clients consume the received query results
      multilocally clients \_ unwrap -> pure $ unwrap results
      pure $ wrap ()

  pure ()

-- Instantiating the choreography

withProxy :: String -> (forall s. KnownSymbol s => Proxy s -> a) -> a
withProxy s f = withSomeSSymbol s \(ss :: SSymbol s) -> withKnownSymbol ss $ f @s Proxy

withProxies :: [String] -> (forall ss. All KnownSymbol ss => NP Proxy ss -> a) -> a
withProxies [] f = f Nil
withProxies (s : ss) f =
  withProxy s \s ->
    withProxies ss \ss ->
      f $ s :* ss

cleanRoomLocs :: Int -> [LocTm]
cleanRoomLocs numClients = server : clients
  where
    server = "s"
    clients = fmap (\i -> "c" <> show i) [1..numClients]

mkCleanRoom :: Int -> [LocTm] -> Cond_' -> Choreo IO ()
mkCleanRoom numFailures (server : clients) cond_' =
  cleanRoom cond_' numFailures & withProxy server & withProxies clients
mkCleanRoom _ [] _ = error "Empty locs"
