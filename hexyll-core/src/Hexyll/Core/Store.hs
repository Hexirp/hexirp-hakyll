{-# LANGUAGE GADTs #-}

module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Typeable ( Typeable )
  import Data.Binary   ( Binary )

  type StoreKey = String

  data StoreValue where
    StoreValue :: (Binary a, Typeable a) => a -> StoreValue

  class Monad m => MonadStore m where
    save :: StoreKey -> StoreValue -> m ()
    loadDelay :: StoreKey -> m (Maybe (m StoreValue))

  load :: MonadStore m => StoreKey -> m (Maybe StoreValue)
  load sk = do
    mmsv <- loadDelay sk
    case mmsv of
      Nothing -> return Nothing
      Just msv -> msv

  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    mmsv <- loadDelay sk
    return $ isJust mmsv
