-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad(replicateM)
import Data.Foldable(fold)
import Data.Function((&))
import Data.Proxy(Proxy(..))
import Data.SOP.BasicFunctors(mapKK, K (..))
import Data.SOP.Classes(HCollapse(..), hmap, hsequenceK)
import Data.SOP.Constraint(All)
import Data.SOP.NP(NP(..))
import GHC.TypeLits(KnownSymbol, SSymbol, withKnownSymbol, withSomeSSymbol)
import System.Environment(getArgs)
import System.Random(randomIO, randomRIO)

import Choreography
import Choreography.Location
import Choreography.Location.Multi

-- Schema and dataset handling (illustrative)

-- For simplicity, all datasets follow the same trivial schema: a list of numbers
data Schema = Schema
  deriving stock (Show, Read)

newtype Dataset = Dataset [Int]
  deriving stock (Show, Read)
  deriving newtype (Semigroup, Monoid)

data Query
  = Len
  | Sum
  | Avg
  | Min
  | Max
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

loadSchema :: Proxy l -> IO Schema
loadSchema _ = pure Schema

mergeSchemas :: [Schema] -> Schema
mergeSchemas _ = Schema

conformsToSchema :: Schema -> Dataset -> IO Bool
conformsToSchema _ _ = randomIO -- To simulate failures

emptyDataset :: Dataset
emptyDataset = Dataset []

-- Simulate some I/O to load a dataset (e.g. from a file)
loadDataset :: Proxy l -> IO Dataset
loadDataset _ = do
  n <- randomRIO (5, 10)
  Dataset <$> replicateM n (randomRIO (-100, 100))

mergeDatasets :: [Dataset] -> Dataset
mergeDatasets = fold

-- Simulate the selection of a query (by picking one randomly)
loadQuery :: Proxy l -> Schema -> IO Query
loadQuery _ _ = toEnum <$> randomRIO (fromEnum @Query minBound, fromEnum @Query maxBound)

agreeWithQueries :: Proxy l -> [Query] -> IO Bool
agreeWithQueries _ _ = pure True

runQuery :: Dataset -> Query -> IO Int
runQuery (Dataset ds) q =
  pure $
    case q of
      Len -> length ds
      Sum -> sum ds
      Avg -> sum ds `quot` length ds
      Min -> minimum ds
      Max -> maximum ds

-- Error-handling (if datasets don't conform to schema)

getClientDataset
  :: (KnownSymbol server, KnownSymbol client)
  => Int
  -> Schema @ server
  -> Proxy server
  -> Proxy client
  -> Choreo IO (Dataset @ server)
getClientDataset tries schema server client =
  if tries <= 0 then
    pure $ wrap emptyDataset
  else do
    dataset <- (client, \_ -> loadDataset client) ~~> server
    cond_' (server, \unwrap -> conformsToSchema (unwrap schema) (unwrap dataset)) \case
      True -> pure dataset
      False -> getClientDataset (tries - 1) schema server client

getClientDatasets
  :: (KnownSymbol server, All KnownSymbol clients)
  => Int
  -> Schema @ server
  -> Proxy server
  -> NP Proxy clients
  -> Choreo IO (NP (K Dataset) clients @ server)
getClientDatasets _ _ _ Nil = pure $ wrap Nil
getClientDatasets tries schema server (client :* clients) = do
  dataset <- getClientDataset tries schema server client
  datasets <- getClientDatasets tries schema server clients
  locally server \unwrap -> pure $ K (unwrap dataset) :* unwrap datasets

-- Main choreography

cleanRoom
  :: (KnownSymbol server, All KnownSymbol clients)
  => Proxy server
  -> NP Proxy clients
  -> Choreo IO ()
cleanRoom server clients = do
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
      multilocally clients \_ _ -> print "Disagreement"
      pure $ wrap ()
    True -> do -- Everyone agrees, proceed
      -- The amount of times the server will ask for a valid dataset
      let tries = 5

      -- Clients send their datasets to the server
      datasets <- getClientDatasets tries finalSchemaS server clients

      -- Server locally merges the datasets and runs all the queries
      resultsS <- locally server \unwrap -> do
        let aggregatedDataset = mergeDatasets $ hcollapse $ unwrap datasets
        hsequenceK $ hmap (mapKK $ runQuery aggregatedDataset) $ unwrap queriesS

      -- Server sends the result of each query "pointwise"
      results <- (server, resultsS) ~>. clients

      -- Clients consume the received query results
      multilocally clients \_ unwrap -> print $ unwrap results
      pure $ wrap ()

  pure ()

-- Running the choreography

withProxy :: String -> (forall s. KnownSymbol s => Proxy s -> a) -> a
withProxy s f = withSomeSSymbol s \(ss :: SSymbol s) -> withKnownSymbol ss $ f @s Proxy

withProxies :: [String] -> (forall ss. All KnownSymbol ss => NP Proxy ss -> a) -> a
withProxies [] f = f Nil
withProxies (s : ss) f =
  withProxy s \s ->
    withProxies ss \ss ->
      f $ s :* ss

httpConfig :: [String] -> HttpConfig
httpConfig ls = mkHttpConfig [(l, ("localhost", port)) | l <- ls | port <- [3000..]]

main :: IO ()
main = do
  self : ls@(server : clients) <- getArgs
  runChoreography (httpConfig ls) (cleanRoom & withProxy server & withProxies clients) self
