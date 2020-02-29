{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Typeable ( Typeable, typeOf, cast, TypeRep )
  import Data.Binary   ( Binary )

  type StoreKey = String

  data StoreValue where
    MkStoreValue :: (Binary a, Typeable a) => a -> StoreValue

  deStoreValue
    :: StoreValue
    -> (forall a. (Binary a, Typeable a) => a -> r)
    -> r
  deStoreValue (MkStoreValue x) f = f x

  unwrapStoreValue :: Typeable a => StoreValue -> Either TypeRep a
  unwrapStoreValue (MkStoreValue x) = case cast x of
    Nothing -> Left (typeOf x)
    Just a -> Right a

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
