{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )
  import Data.Proxy ( Proxy (..), asProxyTypeOf )

  import Data.Typeable ( Typeable, typeOf, typeRep, cast, TypeRep )
  import Data.Binary   ( Binary (..), Put, Get )

  type StoreKey = String

  data StoreValue where
    MkStoreValue :: (Binary a, Typeable a) => a -> StoreValue

  deStoreValue
    :: StoreValue
    -> (forall a. (Binary a, Typeable a) => a -> r)
    -> r
  deStoreValue (MkStoreValue x) f = f x

  unwrapStoreValue :: Typeable a => StoreValue -> Maybe a
  unwrapStoreValue (MkStoreValue x) = cast x

  putStoreValue :: (Binary a, Typeable a) -> a -> Put
  putStoreValue x = do
    put (typeOf x)
    put x

  getStoreValue
    :: (Binary a, Typeable a) => Proxy a -> Get (Either StoreError a)
  getStoreValue proxy = let trExpect = typeRep proxy in do
    trActual <- get
    if trActual == trExpect
      then do
        x <- get
        return $ Right x
      else
        return $ Left (StoreError trExpect trActual)

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
    StoreResult mmesv <- loadDelay @_ @() sk
    return $ isJust mmesv
