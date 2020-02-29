{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )
  import Data.Proxy ( Proxy (..), asProxyTypeOf )

  import Data.Typeable ( Typeable, typeOf, cast, TypeRep )
  import Data.Binary   ( Binary (..) )

  type StoreKey = String

  newtype StoreResult m a = StoreResult
    { unStoreResult :: Maybe (m (Either StoreError a))
    } deriving ( Typeable )

  data StoreError = StoreError
    { storeExpect :: TypeRep
    , storeActual :: TypeRep
    } deriving ( Eq, Show, Typeable )

  class Monad m => MonadStore m where
    save :: (Binary a, Typeable a) => StoreKey -> a -> m ()
    loadDelay :: (Binary a, Typeable a) => StoreKey -> m (StoreResult m a)

  load
    :: (Binary a, Typeable a, MonadStore m)
    => StoreKey
    -> m (Maybe (Either StoreError a))
  load sk = do
    StoreResult mmesv <- loadDelay sk
    case mmesv of
      Nothing -> return Nothing
      Just mesv -> Just <$> mesv

  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    StoreResult mmesv <- loadDelay sk
    let _type_info = StoreResult mmesv :: StoreResult m ()
    return $ isJust mmesv
