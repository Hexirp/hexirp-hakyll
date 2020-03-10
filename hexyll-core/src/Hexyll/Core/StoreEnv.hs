{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.StoreEnv where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import Path

  import Hexyll.Core.Store

  data StoreEnv = StoreEnv
    { storeSave :: !(StoreKey -> StoreValue -> IO ())
    , storeLoadDelay :: !(StoreKey -> IO (Maybe (StoreLoad IO)))
    } deriving Typeable

  class HasStoreEnv env where
    storeEnvL :: Lens' env StoreEnv

  saveE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> StoreValue
    -> m ()
  saveE sk sv = do
    env <- ask
    liftIO $ storeSave (view storeEnvL env) sk sv

  loadDelayE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> m (Maybe (StoreLoad m))
  loadDelayE sk = do
    env <- ask
    liftIO $ fmap (fmap (mapStoreLoad liftIO)) $
      storeLoadDelay (view storeEnvL env) sk

  data StoreOption = StoreOption
    { storeLocation :: !(Path Rel Dir)
    , storeInMemory :: !Bool
    } deriving (Eq, Ord, Show, Typeable)

  newStoreEnv :: StoreOption -> IO StoreEnv
  newStoreEnv (StoreOption sl si) = if si
    then newStoreEnvInMemory sl
    else newStoreEnvNoMemory sl

  newStoreEnvInMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvInMemory = undefined

  newStoreEnvNoMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvNoMemory = undefined
