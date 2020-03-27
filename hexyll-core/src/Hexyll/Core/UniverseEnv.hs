-- |
-- Module:      Hexyll.Core.UniverseEnv
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides an environment for handling the 'Identifier' set.
--
-- @since 0.1.0.0
module Hexyll.Core.UniverseEnv where

  import Prelude

  import Data.Typeable  ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import qualified Data.Set as S

  import Hexyll.Core.Identifier
  import Hexyll.Core.Universe

  -- | The type of environment for handling the 'Identifier' set.
  --
  -- @since 0.1.0.0
  data UniverseEnv = UniverseEnv
    { universeGetMatches :: !(Pattern -> IO (S.Set Identifier))
    , universeGetAllIdentifier :: !(IO (S.Set Identifier))
    , universeCountUniverse :: !(IO Int)
    } deriving Typeable

  -- | Environment values with an environment for handling the 'Identifier'
  -- set.
  --
  -- @since 0.1.0.0
  class HasUniverseEnv env where
    universeEnvL :: Lens' env UniverseEnv

  -- | @since 0.1.0.0
  instance HasUniverseEnv UniverseEnv where
    universeEnvL = id

  -- | Get identifiers that matches the pattern.
  --
  -- @since 0.1.0.0
  getMatchesE
    :: (MonadIO m, MonadReader env m, HasUniverseEnv env)
    => Pattern -> m (S.Set Identifier)
  getMatchesE p = do
    env <- ask
    liftIO $ universeGetMatches (view universeEnvL env) p

  -- | Get all identifiers.
  --
  -- @since 0.1.0.0
  getAllIdentifier
    :: (MonadIO m, MonadReader env m, HasUniverseEnv env)
    => m (S.Set Identifier)
  getAllIdentifier = do
    env <- ask
    liftIO $ universeGetAllIdentifier (view universeEnvL env)

  -- | Count the number of all identifiers.
  --
  -- @since 0.1.0.0
  countUniverse
    :: (MonadIO m, MonadReader env m, HasUniverseEnv env)
    => m Int
  countUniverse = do
    env <- ask
    liftIO $ universeCountUniverse (view universeEnvL env)
