{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )
  import Data.Proxy ( Proxy (..), asProxyTypeOf )

  import Data.Typeable ( Typeable, typeOf, cast, TypeRep )
  import Data.Binary   ( Binary (..) )

  reflectTypeRep
    :: TypeRep
    -> (forall a. Typeable a => Proxy a -> r)
    -> r
  reflectTypeRep = undefined

  type StoreKey = String

  data StoreValue where
    MkStoreValue :: (Binary a, Typeable a) => a -> StoreValue

  instance Binary StoreValue where
    put (MkStoreValue x) = do
      put (typeOf x)
      put x
    get = do
      tr <- get
      reflectTypeRep tr $ \proxy -> do
        x <- get
        _ <- x `asProxyTypeOf` proxy
        return $ MkStoreValue 

  deStoreValue
    :: StoreValue
    -> (forall a. (Binary a, Typeable a) => a -> r)
    -> r
  deStoreValue (MkStoreValue x) f = f x

  unwrapStoreValue :: Typeable a => StoreValue -> Maybe a
  unwrapStoreValue (MkStoreValue x) = cast x

  class Monad m => MonadStore m where
    save :: StoreKey -> StoreValue -> m ()
    loadDelay :: StoreKey -> m (Maybe (m StoreValue))

  load :: MonadStore m => StoreKey -> m (Maybe StoreValue)
  load sk = do
    mmsv <- loadDelay sk
    case mmsv of
      Nothing -> return Nothing
      Just msv -> Just <$> msv

  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    mmsv <- loadDelay sk
    return $ isJust mmsv
