{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.ProviderEnv
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides an environment for handling a provider.
--
-- @since 0.1.0.0
module Hexyll.Core.ProviderEnv where

  import Prelude

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens', lens )
  import Lens.Micro.Extras ( view )

  import qualified Data.Set as S

  import Path

  import Hexyll.Core.StoreEnv
  import Hexyll.Core.Provider

  -- | A short alias for 'ModificationTime'.
  --
  -- @since 0.1.0.0
  type MTime = ModificationTime

  -- | The type of environment for handling a provider.
  --
  -- @since 0.1.0.0
  data ProviderEnv = ProviderEnv
    { providerGetAllPath
        :: !(StoreEnv -> IO (S.Set (Path Rel File)))
    , providerGetMTimeDelay
        :: !(StoreEnv -> Path Rel File -> IO (Maybe (ProviderLoad IO MTime)))
    , providerGetBodyDelay
        :: !(StoreEnv -> Path Rel File -> IO (Maybe (ProviderLoad IO Body)))
    , providerStore
        :: !StoreEnv
    }

  -- | @since 0.1.0.0
  instance HasStoreEnv ProviderEnv where
    storeEnvL =
      lens providerStore (\env store -> env { providerStore = store })

  -- | Environment values with functions handling a provider.
  --
  -- @since 0.1.0.0
  class HasProviderEnv env where
    providerEnvL :: Lens' env ProviderEnv

  -- | @since 0.1.0.0
  instance HasProviderEnv ProviderEnv where
    providerEnvL = id

  -- | Get all paths.
  --
  -- @since 0.1.0.0
  getAllPathE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => m (S.Set (Path Rel File))
  getAllPathE = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        providerGetAllPath providerEnv (providerStore providerEnv)

  -- | Get the modification time of a file lazily.
  --
  -- @since 0.1.0.0
  getModificationTimeDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))
  getModificationTimeDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetMTimeDelay providerEnv (providerStore providerEnv) p

  -- | Get the body of a file lazily.
  --
  -- @since 0.1.0.0
  getBodyDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m Body))
  getBodyDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetBodyDelay providerEnv (providerStore providerEnv)  p