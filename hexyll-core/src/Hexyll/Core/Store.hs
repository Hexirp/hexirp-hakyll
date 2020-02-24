{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Binary   ( Binary )
  import Data.Typeable ( Typeable, TypeRep )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  type StoreKey = [String]

  data StoreResult a = StoreFound a | StoreNotFound | StoreWrongType TypeRep TypeRep
    deriving (Eq, Show, Typeable)

  data StoreEnv = StoreEnv
    { storeSet :: !(forall a. (Binary a, Typeable a) => StoreKey -> a -> IO ())
    , storeGet :: !(forall a. (Binary a, Typeable a) => StoreKey -> IO (StoreResult a))
    } deriving (Typeable)

  class HasStoreEnv env where
    storeEnvL :: Lens' env StoreEnv

  set
    :: (MonadIO m, MonadReader env m, HasStoreEnv env, Binary a, Typeable a)
    => StoreKey
    -> a
    -> m ()
  set sk x = do
    env <- ask
    liftIO $ storeSet (view storeEnvL env) sk x

  get
    :: (MonadIO m, MonadReader env m, HasStoreEnv env, Binary a, Typeable a)
    => StoreKey
    -> m (StoreResult a)
  set sk = do
    env <- ask
    liftIO $ storeGet (view storeEnvL env) sk
