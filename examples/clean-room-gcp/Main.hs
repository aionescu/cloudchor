-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

-- | Data clean room protocol running over Google Cloud Pub/Sub.
--
-- This is identical to the @clean-room@ example except the HTTP backend is
-- replaced by 'PubSubConfig'.  Each participant communicates through GCP
-- Pub/Sub topics rather than direct HTTP connections, so participants can
-- run on separate machines anywhere in the world without any shared
-- network or open ports.
--
-- Usage:
--
-- @
-- clean-room-gcp \<self\> \<gcp-project-id\> \<server\> \<client1\> [\<client2\> …]
-- @
module Main where

import Control.Monad (replicateM)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Proxy (Proxy (..))
import Data.SOP.BasicFunctors (K (..), mapKK)
import Data.SOP.Classes (HCollapse (..), hmap, hsequenceK)
import Data.SOP.Constraint (All)
import Data.SOP.NP (NP (..))
import Data.Text qualified as Text
import GHC.TypeLits (KnownSymbol, SSymbol, withKnownSymbol, withSomeSSymbol)
import Gogol.Env (newEnv)
import System.Environment (getArgs)
import System.Random (randomIO, randomRIO)

import Choreography
import Choreography.Location
import Choreography.Location.Multi
import Choreography.Network.PubSub

-- * Schema and dataset handling (illustrative)
--
-- For simplicity, all datasets follow the same trivial schema: a list of numbers.

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
conformsToSchema _ _ = randomIO -- simulate validation failures

emptyDataset :: Dataset
emptyDataset = Dataset []

loadDataset :: Proxy l -> IO Dataset
loadDataset _ = do
  n <- randomRIO (5, 10)
  Dataset <$> replicateM n (randomRIO (-100, 100))

mergeDatasets :: [Dataset] -> Dataset
mergeDatasets = fold

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

-- * Choreography (unchanged from the HTTP version)

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
      True  -> pure dataset
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
  dataset  <- getClientDataset tries schema server client
  datasets <- getClientDatasets tries schema server clients
  locally server \unwrap -> pure $ K (unwrap dataset) :* unwrap datasets

cleanRoom
  :: (KnownSymbol server, All KnownSymbol clients)
  => Proxy server
  -> NP Proxy clients
  -> Choreo IO ()
cleanRoom server clients = do
  schemas      <- (clients, \c _ -> loadSchema c) *~~> server
  finalSchemaS <- locally server \unwrap ->
                    pure $ mergeSchemas $ hcollapse $ unwrap schemas
  finalSchema  <- (server, finalSchemaS) ~>* clients
  queriesS     <- (clients, \c unwrap -> loadQuery c $ unwrap finalSchema) *~~> server
  queries      <- (server, \unwrap -> pure $ hcollapse $ unwrap queriesS) ~~>* clients
  agreements   <- (clients, \c unwrap -> agreeWithQueries c $ unwrap queries) *~~> server

  cond_' (server, \unwrap -> pure $ and $ hcollapse $ unwrap agreements) \case
    False -> do
      multilocally clients \_ _ -> print "Disagreement"
      pure $ wrap ()
    True -> do
      datasets  <- getClientDatasets 5 finalSchemaS server clients
      resultsS  <- locally server \unwrap -> do
                     let agg = mergeDatasets $ hcollapse $ unwrap datasets
                     hsequenceK $ hmap (mapKK $ runQuery agg) $ unwrap queriesS
      results   <- (server, resultsS) ~>. clients
      multilocally clients \_ unwrap -> print $ unwrap results
      pure $ wrap ()

  pure ()

-- * Entry point

withProxy :: String -> (forall s. KnownSymbol s => Proxy s -> a) -> a
withProxy s f = withSomeSSymbol s \(ss :: SSymbol s) -> withKnownSymbol ss $ f @s Proxy

withProxies :: [String] -> (forall ss. All KnownSymbol ss => NP Proxy ss -> a) -> a
withProxies []       f = f Nil
withProxies (s : ss) f = withProxy s \p -> withProxies ss \ps -> f (p :* ps)

-- | Build a 'PubSubConfig' for the given GCP project and participant list.
-- Topics and subscriptions must already exist (see the README).
mkConfig :: Text.Text -> [String] -> IO PubSubConfig
mkConfig project ls = do
  env <- newEnv   -- reads Application Default Credentials from the environment
  pure $ mkPubSubConfig env project ls

-- | Args: \<self\> \<gcp-project-id\> \<server\> \<client1\> [\<client2\> …]
main :: IO ()
main = do
  self : project : ls@(server : clients) <- getArgs
  cfg <- mkConfig (Text.pack project) ls
  runChoreography cfg (cleanRoom & withProxy server & withProxies clients) self
