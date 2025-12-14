{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}

-- | This module defines `Choreo`, the monad for writing choreographies.
module Choreography.Choreo where

import Control.Applicative(empty)
import Control.Monad.State.Strict(StateT, execStateT, modify')
import Data.Foldable(foldMap')
import Data.Functor(($>))
import Data.List(delete)
import Data.Maybe(fromMaybe)
import Data.Proxy(Proxy(..))
import Data.Set(Set)
import Data.Set qualified as S
import GHC.TypeLits(KnownSymbol)

import Choreography.Location
import Choreography.Network
import Control.Monad.Freer

-- * The Choreo monad

-- | A constrained version of `unwrap` that only unwraps values located at a
-- specific location.
type Unwrap l = forall a. a @ l -> a

-- | Effect signature for the `Choreo` monad. @m@ is a monad that represents
-- local computations.
data ChoreoSig m a where
  Local :: (KnownSymbol l)
        => Proxy l
        -> (Unwrap l -> m a)
        -> ChoreoSig m (a @ l)

  Comm :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
       => Proxy l
       -> a @ l
       -> Proxy l'
       -> ChoreoSig m (a @ l')

  Cond :: (Show a, Read a, KnownSymbol l)
       => Proxy l
       -> a @ l
       -> (a -> Choreo m b)
       -> ChoreoSig m b

  Cond_ :: (Bounded a, Enum a, Show a, Read a, KnownSymbol l)
        => Proxy l
        -> a @ l
        -> (a -> Choreo m (b @ l))
        -> ChoreoSig m (b @ l)

-- | Monad for writing choreographies.
type Choreo m = Freer (ChoreoSig m)

-- | Run a `Choreo` monad directly.
runChoreo :: Monad m => Choreo m a -> m a
runChoreo = interpFreer handler
  where
    handler :: Monad m => ChoreoSig m a -> m a
    handler (Local _ m)   = wrap <$> m unwrap
    handler (Comm _ a _)  = return $ (wrap . unwrap) a
    handler (Cond _ a c)  = runChoreo $ c (unwrap a)
    handler (Cond_ _ a c) = runChoreo $ c (unwrap a)

-- Static analysis

insertLoc :: KnownSymbol l => Proxy l -> Set LocTm -> Set LocTm
insertLoc = S.insert . toLocTm

insertLocs :: [LocTm] -> Set LocTm -> Set LocTm
insertLocs = S.union . S.fromList

-- | Apply function `f` to every possible value of type `a`, and collect all the resulting `participants`.
enumerate :: (Bounded a, Enum a) => [LocTm] -> (a -> Choreo m b) -> Set LocTm
enumerate ls f = foldMap' (participants ls . f) [minBound ..]

-- | Compute the set of locations participating in this choreography.
participants :: [LocTm] -> Choreo m a -> Set LocTm
participants ls = fromMaybe (S.fromList ls) . (`execStateT` S.empty) . interpFreer handler
  where
    handler :: ChoreoSig m a -> StateT (Set LocTm) Maybe a
    handler = \case
      Local l _   -> modify' (insertLoc l)                $> Empty
      Comm l _ l' -> modify' (insertLoc l . insertLoc l') $> Empty
      Cond _ _ _  -> modify' (insertLocs ls)              *> empty
      Cond_ l _ f -> modify' (insertLoc l . S.union (enumerate ls f)) $> Empty

-- | Endpoint projection.
epp :: [LocTm] -> Choreo m a -> LocTm -> Network m a
epp ls c l' = interpFreer handler c
  where
    handler :: ChoreoSig m a -> Network m a
    handler (Local l m)
      | toLocTm l == l' = wrap <$> run (m unwrap)
      | otherwise       = return Empty
    handler (Comm s a r)
      | l' == s' && s' == r' = return $ wrap (unwrap a)
      | s' == l'             = send (unwrap a) r' >> return Empty
      | r' == l'             = wrap <$> recv s'
      | otherwise            = return Empty
      where
        s' = toLocTm s
        r' = toLocTm r
    handler (Cond l a c)
      | toLocTm l == l' = broadcast (unwrap a) (delete l' ls) >> epp ls (c (unwrap a)) l'
      | otherwise       = recv (toLocTm l) >>= \x -> epp ls (c x) l'
    handler (Cond_ pl a c)
      | l == l'         = broadcast (unwrap a) (S.toList $ S.delete l ls') >> epp ls (c (unwrap a)) l'
      | S.member l' ls' = recv l >>= \x -> epp ls (c x) l'
      | otherwise       = pure Empty
      where
        l = toLocTm pl
        ls' = enumerate ls c

-- * Choreo operations

-- | Perform a local computation at a given location.
locally :: KnownSymbol l
        => Proxy l           -- ^ Location performing the local computation.
        -> (Unwrap l -> m a) -- ^ The local computation given a constrained
                             -- unwrap funciton.
        -> Choreo m (a @ l)
locally l m = toFreer (Local l m)

-- | Communication between a sender and a receiver.
(~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
     => (Proxy l, a @ l)  -- ^ A pair of a sender's location and a value located
                          -- at the sender
     -> Proxy l'          -- ^ A receiver's location.
     -> Choreo m (a @ l')
(~>) (l, a) l' = toFreer (Comm l a l')

-- | Conditionally execute choreographies based on a located value.
cond :: (Show a, Read a, KnownSymbol l)
     => (Proxy l, a @ l)  -- ^ A pair of a location and a scrutinee located on
                          -- it.
     -> (a -> Choreo m b) -- ^ A function that describes the follow-up
                          -- choreographies based on the value of scrutinee.
     -> Choreo m b
cond (l, a) c = toFreer (Cond l a c)

-- | Conditionally execute choreographies at participating locations,
-- based on a located value.
cond_ :: (Bounded a, Enum a, Show a, Read a, KnownSymbol l)
      => (Proxy l, a @ l)        -- ^ A pair of a location and a scrutinee located on
                                 -- it.
      -> (a -> Choreo m (b @ l)) -- ^ A function that describes the follow-up
                                 -- choreographies based on the value of scrutinee.
      -> Choreo m (b @ l)
cond_ (l, a) c = toFreer (Cond_ l a c)

-- | A variant of `~>` that sends the result of a local computation.
(~~>) :: (Show a, Read a, KnownSymbol l, KnownSymbol l')
      => (Proxy l, Unwrap l -> m a) -- ^ A pair of a sender's location and a local
                                    -- computation.
      -> Proxy l'                   -- ^ A receiver's location.
      -> Choreo m (a @ l')
(~~>) (l, m) l' = do
  x <- l `locally` m
  (l, x) ~> l'

-- | A variant of `cond` that conditonally executes choregraphies based on the
-- result of a local computation.
cond' :: (Show a, Read a, KnownSymbol l)
      => (Proxy l, Unwrap l -> m a) -- ^ A pair of a location and a local
                                    -- computation.
      -> (a -> Choreo m b)          -- ^ A function that describes the follow-up
                                    -- choreographies based on the result of the
                                    -- local computation.
      -> Choreo m b
cond' (l, m) c = do
  x <- l `locally` m
  cond (l, x) c

-- | A variant of `cond_` that conditonally executes choregraphies at participating locations,
-- based on the result of a local computation.
cond_' :: (Bounded a, Enum a, Show a, Read a, KnownSymbol l)
       => (Proxy l, Unwrap l -> m a) -- ^ A pair of a location and a local
                                     -- computation.
       -> (a -> Choreo m (b @ l))    -- ^ A function that describes the follow-up
                                     -- choreographies based on the result of the
                                     -- local computation.
       -> Choreo m (b @ l)
cond_' (l, m) c = do
  x <- l `locally` m
  cond_ (l, x) c
