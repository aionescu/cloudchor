{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines the interface to HasChor. The client of the library is
-- highly recommended to only use constructs exported by this module.
module Choreography (
  -- * Locations and Located Values
  LocTm,
  LocTy,
  type (@),
  mkLoc,

  -- * The Choreo monad
  Choreo,
  -- ** Choreo operations
  locally,
  (~>),
  (~~>),
  cond,
  cond',
  cond_,
  cond_',

  -- * Message transport backends
  -- ** The HTTP backend
  Host,
  Port,
  HttpConfig,
  mkHttpConfig,

  -- * Running choreographies
  runChoreo,
  runChoreography
  ) where

import Choreography.Location
import Choreography.Choreo
import Choreography.Network
import Choreography.Network.Http
import Control.Monad.IO.Class

-- | Run a choreography with a message transport backend.
runChoreography :: (Backend config, MonadIO m) => config -> Choreo m a -> LocTm -> m a
runChoreography cfg choreo l = runNetwork cfg l (epp (locs cfg) choreo l)
