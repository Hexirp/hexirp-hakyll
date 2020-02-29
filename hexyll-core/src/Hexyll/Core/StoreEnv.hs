{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.StoreEnv where

  import Prelude

  import Data.Typeable ( Typeable, TypeRep )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import Hexyll.Core.Store

  data StoreEnv = StoreEnv
    { storeSave :: !(StoreKey -> StoreValue -> IO ())
    , storeLoadDelay :: !(StoreKey -> IO (Maybe (StoreLoad IO)))
    } deriving Typeable

  class HasStoreEnv env where
    storeEnvL :: Lens' env StoreEnv

  saveEnv
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> StoreValue
    -> m ()
  saveEnv sk sv = do
    env <- ask
    liftIO $ storeSave (view storeEnvL env) sk sv

  loadDelayEnv
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> m (Maybe (StoreLoad m))
  loadDelayEnv sk = do
    env <- ask
    liftIO $ fmap (fmap (mapStoreLoad liftIO)) $
      storeLoadDelay (view storeEnvL env) sk
