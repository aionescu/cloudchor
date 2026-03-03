-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

-- | This module implements the Google Cloud Pub/Sub message transport backend
-- for the `Network` monad.
--
-- Each choreographic location is mapped to a dedicated Pub/Sub topic (inbox)
-- and a pull subscription. The location name is embedded as a message
-- attribute and ordering key, so the pull thread can route incoming messages
-- to the correct per-sender 'Chan', replicating the structure of the HTTP
-- backend.
--
-- === GCP setup (per location @l@)
--
-- * Topic:        @projects\/{project}\/topics\/{l}@
-- * Subscription: @projects\/{project}\/subscriptions\/{l}@
--   attached to the topic above, with __message ordering enabled__.
--
-- === Authentication
--
-- Uses Application Default Credentials. Run @gcloud auth application-default
-- login@ locally, or attach a service account when running on GCP.
{-# LANGUAGE OverloadedStrings #-}
module Choreography.Network.PubSub where

import Choreography.Location
import Choreography.Network (Backend (..), Network, NetworkSig (..))
import Control.Concurrent
import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int32)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import Gogol qualified as G
import Gogol.Env (Env)
import Gogol.Prelude (Base64 (..))
import Gogol.PubSub.Projects.Subscriptions.Acknowledge
import Gogol.PubSub.Projects.Subscriptions.Pull
import Gogol.PubSub.Projects.Topics.Publish
import Gogol.PubSub.Types
  ( AcknowledgeRequest (..)
  , newAcknowledgeRequest
  , PublishRequest (..)
  , newPublishRequest
  , PubsubMessage (..)
  , PubsubMessage_Attributes (..)
  , newPubsubMessage
  , PullRequest (..)
  , newPullRequest
  , PullResponse (..)
  , ReceivedMessage (..)
  )

-- * Scopes

type PubSubScopes = '["https://www.googleapis.com/auth/pubsub"]

-- * Configuration

-- | The Pub/Sub backend configuration.
data PubSubConfig = PubSubConfig
  { pubSubEnv  :: Env PubSubScopes
    -- ^ Google Cloud credentials and HTTP manager.
  , locToTopic :: HashMap LocTm Text
    -- ^ Maps each location to its inbox topic resource name.
  , locToSub   :: HashMap LocTm Text
    -- ^ Maps each location to its pull subscription resource name.
  }

-- | Build a 'PubSubConfig' from an 'Env' and a GCP project ID.
-- Topics and subscriptions are expected to follow the naming convention:
--
-- @
-- projects\/{project}\/topics\/{location}
-- projects\/{project}\/subscriptions\/{location}
-- @
mkPubSubConfig :: Env PubSubScopes -> Text -> [LocTm] -> PubSubConfig
mkPubSubConfig env project locs = PubSubConfig
  { pubSubEnv  = env
  , locToTopic = mk "topics"        locs
  , locToSub   = mk "subscriptions" locs
  }
  where
    mk kind ls =
      HashMap.fromList
        [(l, "projects/" <> project <> "/" <> kind <> "/" <> Text.pack l) | l <- ls]

locsPubSub :: PubSubConfig -> [LocTm]
locsPubSub = HashMap.keys . locToTopic

-- * Receiving channels

-- | Per-sender message buffers, keyed by sender location name.
type RecvChans = HashMap LocTm (Chan String)

mkRecvChans :: [LocTm] -> IO RecvChans
mkRecvChans = foldM f HashMap.empty
  where
    f hm l = do
      c <- newChan
      return $ HashMap.insert l c hm

-- * Pub/Sub backend

runNetworkPubSub :: MonadIO m => PubSubConfig -> LocTm -> Network m a -> m a
runNetworkPubSub cfg self prog = do
  chans  <- liftIO $ mkRecvChans (locsPubSub cfg)
  pullT  <- liftIO $ forkIO (pullThread cfg self chans)
  result <- interpFreer (handler chans) prog
  liftIO  $ killThread pullT
  return result
  where
    env = pubSubEnv cfg

    handler :: MonadIO m => RecvChans -> NetworkSig m a -> m a
    handler _     (Run m)      = m
    handler _     (Send a l)   = liftIO $ publishMsg env (locToTopic cfg ! l) self (show a)
    handler chans (Recv l)     = liftIO $ read <$> readChan (chans ! l)
    handler chans (BCast a ls) = mapM_ (handler chans) $ fmap (Send a) ls

-- | Publish a single serialized message to @topic@, tagging it with the
-- sender location as both a message attribute and an ordering key.
-- Ordering keys ensure per-sender FIFO delivery at the subscriber.
publishMsg :: Env PubSubScopes -> Text -> LocTm -> String -> IO ()
publishMsg env topic sender body = runResourceT $ do
  let msg = (newPubsubMessage :: PubsubMessage)
        { data'       = Just (Base64 (BS.pack body))
        , attributes  = Just (PubsubMessage_Attributes
            (HashMap.singleton "sender" (Text.pack sender)))
        , orderingKey = Just (Text.pack sender)
        }
      req = newPublishRequest { messages = Just [msg] }
  res <- G.sendEither env (newPubSubProjectsTopicsPublish req topic)
  case res of
    Left err -> liftIO $ putStrLn $ "PubSub publish error: " ++ show err
    Right _  -> return ()

-- | Long-polling loop that pulls messages from @self@'s subscription, routes
-- each one to the appropriate per-sender 'Chan', and batch-acknowledges them.
-- Runs in a dedicated thread for the lifetime of 'runNetworkPubSub'.
pullThread :: PubSubConfig -> LocTm -> RecvChans -> IO ()
pullThread cfg self chans = forever $ runResourceT $ do
  let sub     = locToSub cfg ! self
      pullReq = (newPullRequest :: PullRequest) { maxMessages = Just (10 :: Int32) }
  res <- G.sendEither env (newPubSubProjectsSubscriptionsPull pullReq sub)
  case res of
    Left err   -> liftIO $ putStrLn $ "PubSub pull error: " ++ show err
    Right resp -> do
      let PullResponse maybeReceived = resp
          received = fromMaybe [] maybeReceived
      liftIO $ forM_ received $ \rm ->
        let ReceivedMessage { message = maybeMsg } = rm
        in case maybeMsg of
          Nothing -> return ()
          Just m  -> do
            let PubsubMessage { attributes = maybeAttrs, data' = maybeData } = m
                PubsubMessage_Attributes attrMap =
                  fromMaybe (PubsubMessage_Attributes HashMap.empty) maybeAttrs
                body = maybe "" (\(Base64 bs) -> BS.unpack bs) maybeData
            case HashMap.lookup "sender" attrMap of
              Nothing -> putStrLn "PubSub: message missing sender attribute"
              Just s  -> writeChan (chans ! Text.unpack s) body
      let ids = mapMaybe (\rm -> case rm of ReceivedMessage { ackId = aid } -> aid) received
      unless (null ids) $ do
        let ackReq = (newAcknowledgeRequest :: AcknowledgeRequest) { ackIds = Just ids }
        void $ G.sendEither env
          (newPubSubProjectsSubscriptionsAcknowledge ackReq sub)
  where
    env = pubSubEnv cfg

instance Backend PubSubConfig where
  locs       = locsPubSub
  runNetwork = runNetworkPubSub
