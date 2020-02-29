{-# LANGUAGE GADTs #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Typeable ( Typeable, TypeRep )
  import Data.Binary   ( Binary )

  type StoreKey = String

  data StoreValue where
    StoreValue :: (Binary a, Typeable a) => a -> StoreValue

  storeValue :: StoreValue -> (forall a. (Binary a, Typeable a) => a -> r) -> r
  storeValue (StoreValue x) f = f x

  unwrapStValue :: Typeable a => StoreValue -> Either TypeRep a
  unwrapStValue (StoreValue x) = case cast x of
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
