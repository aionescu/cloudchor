-- SPDX-FileCopyrightText: 2025 Alex Ionescu
-- SPDX-License-Identifier: MPL-2.0

-- | This module defines multiply-located values and combinators for manipulating them.
module Choreography.Location.Multi where

import GHC.TypeLits(KnownSymbol)
import Data.Proxy(Proxy(..))
import Data.SOP.BasicFunctors(K(..), mapKK, unK)
import Data.SOP.Classes(HSequence(..), hmap)
import Data.SOP.Constraint(All)
import Data.SOP.NP(NP(..), hd, tl)

import Choreography.Choreo
import Choreography.Location

-- | The type of multiply-located values.
type a @@ ls = NP ((@) a) ls

-- | "scatter": Send a located value from a single location to many others.
(~>*)
  :: (KnownSymbol l, All KnownSymbol ls, Show a, Read a)
  => (Proxy l, a @ l)
  -> NP Proxy ls
  -> Choreo m (a @@ ls)
(l, a) ~>* ls = hctraverse' (Proxy @KnownSymbol) ((l, a) ~>) ls

infix 4 ~>*

-- | "gather": Send a multiply-located value from many locations to one.
(*~>)
  :: (KnownSymbol l, All KnownSymbol ls, Applicative m, Show a, Read a)
  => a @@ ls
  -> Proxy l
  -> Choreo m (NP (K a) ls @ l)
as *~> l = do
  as' <- hctraverse' (Proxy @KnownSymbol) (\a -> K <$> ((Proxy, a) ~> l)) as
  locally l \unwrap -> pure $ hmap (mapKK unwrap) as'

infix 4 *~>

-- | Send a list of values from one location to many others "pointwise" (i.e. each target gets one value).
(~>.)
  :: (KnownSymbol l, All KnownSymbol ls, Applicative m, Show a, Read a)
  => (Proxy l, NP (K a) ls @ l)
  -> NP Proxy ls
  -> Choreo m (a @@ ls)
_       ~>. Nil = pure Nil
(l, as) ~>. (l' :* ls') = do
  a <- locally l \unwrap -> pure $ unK $ hd $ unwrap as
  as' <- locally l \unwrap -> pure $ tl $ unwrap as
  (:*) <$> ((l, a) ~> l') <*> ((l, as') ~>. ls')

infix 4 ~>.

-- | Multiply-located version of `Unwrap`.
type Unwraps ls = forall a. a @@ ls -> a

-- | Multiply-located version of `locally`.
multilocally
  :: forall ls m a
  .  All KnownSymbol ls
  => NP Proxy ls
  -> (forall l. KnownSymbol l => Proxy l -> Unwraps ls -> m a)
  -> Choreo m (a @@ ls)
multilocally ls f = go ls id
  where
    go
      :: forall ls'
      .  All KnownSymbol ls'
      => NP Proxy ls'
      -> (forall a. a @@ ls -> a @@ ls')
      -> Choreo m (a @@ ls')
    go Nil _pick = pure Nil
    go (l :* ls) pick = do
      a <- locally l \unwrap -> f l $ unwrap . hd . pick
      (a :*) <$> go ls (tl . pick)

-- | A variant of `~>*` that sends the result of a local computation.
(~~>*)
  :: (KnownSymbol l, All KnownSymbol ls, Show a, Read a)
  => (Proxy l, Unwrap l -> m a)
  -> NP Proxy ls
  -> Choreo m (a @@ ls)
(l, f) ~~>* ls = do
  a <- locally l f
  (l, a) ~>* ls

infix 4 ~~>*

-- | A variant of `*~>` that sends the result of a local computation.
(*~~>)
  :: (KnownSymbol l, All KnownSymbol ls, Applicative m, Show a, Read a)
  => (NP Proxy ls, forall l. KnownSymbol l => Proxy l -> Unwraps ls -> m a)
  -> Proxy l
  -> Choreo m (NP (K a) ls @ l)
(ls, f) *~~> l = do
  a <- multilocally ls f
  a *~> l

infix 4 *~~>
