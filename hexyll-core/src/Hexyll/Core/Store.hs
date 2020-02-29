{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  putStValRawly :: (Binary a, Typeable a) => a -> Put
  putStValRawly x = do
    put (typeOf x)
    put x

  putStVal :: StoreValue -> Put
  putStVal (MkStoreValue x) = do
    put (typeOf x)
    put x

  getStValRestrictly
    :: (Binary a, Typeable a) => Proxy a -> Get (Either StoreError StoreValue)
  getStValRestrictly proxy = let trExpect = typeRep proxy in do
    trActual <- get
    if trActual == trExpect
      then do
        x <- get
        return $ Right (MkStoreValue x) `const` (x `asProxyTypeOf` proxy)
      else
        return $ Left (StoreError trExpect trActual)

  getWrapStVal
    :: forall a. (Binary a, Typeable a) => Get (Either StoreError a)
  getWrapStVal = let trExpect = typeRep (Proxy @a) in do
    trActual <- get
    if trActual == trExpect
      then do
        x <- get
        return $ Right x
      else
        return $ Left (StoreError trExpect trActual)

  newtype StoreLoad m = StoreLoad
    { runStoreLoad
        :: forall a.
           (Binary a, Typeable a)
        => m (Either StoreError a)
    } deriving ( Typeable )

  data StoreError = StoreError
    { storeExpect :: TypeRep
    , storeActual :: TypeRep
    } deriving ( Eq, Show, Typeable )

  class Monad m => MonadStore m where
    save :: StoreKey -> StoreValue -> m ()
    loadDelay :: StoreKey -> m (Maybe (StoreLoad m))

  load
    :: (Binary a, Typeable a, MonadStore m)
    => StoreKey
    -> m (Maybe (Either StoreError a))
  load sk = do
    msl <- loadDelay sk
    case msl of
      Nothing -> pure Nothing
      Just sl -> Just <$> runStoreLoad sl (Proxy @a)

  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    StoreResult mmesv <- loadDelay @_ @() sk
    return $ isJust mmesv
