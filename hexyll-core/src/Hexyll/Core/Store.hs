{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Hexyll.Core.Store
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: non-portable (ghc-extensions: GADTs and RankNTypes)
--
-- This module provides the basic types and type classes for handling a store.
--
-- @since 0.1.0.0
module Hexyll.Core.Store where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Typeable ( Typeable, cast, TypeRep )
  import Data.Binary   ( Binary (..) )

  -- | A key for 'MonadStore'.
  --
  -- @since 0.1.0.0
  type StoreKey = String

  -- | A value for 'MonadStore'.
  --
  -- @since 0.1.0.0
  data StoreValue where
    MkStoreValue :: (Binary a, Typeable a) => !a -> StoreValue

  -- | Destruct a 'StoreValue'.
  --
  -- @since 0.1.0.0
  deStoreValue
    :: StoreValue
    -> (forall a. (Binary a, Typeable a) => a -> r)
    -> r
  deStoreValue (MkStoreValue x) f = f x

  -- | Unwrap a 'StoreValue' by 'cast'.
  --
  -- @since 0.1.0.0
  unwrapStoreValue :: Typeable a => StoreValue -> Maybe a
  unwrapStoreValue (MkStoreValue x) = cast x

  -- | A type of delayed loading.
  --
  -- @since 0.1.0.0
  newtype StoreLoad m = StoreLoad
    { runStoreLoad
        :: forall a. (Binary a, Typeable a) => m (Either StoreError a)
    } deriving ( Typeable )

  -- | Map over 'StoreLoad'.
  --
  -- @since 0.1.0.0
  mapStoreLoad :: (forall a. m a -> n a) -> StoreLoad m -> StoreLoad n
  mapStoreLoad f (StoreLoad sl) = StoreLoad (f sl)

  -- | An error type for 'StoreLoad'.
  --
  -- @since 0.1.0.0
  data StoreError
    = DecodeError !StoreDecodeError
    | TypeCastError !StoreTypeCastError
    deriving ( Eq, Ord, Show, Typeable )

  -- | An error type when decoding.
  --
  -- @since 0.1.0.0
  newtype StoreDecodeError = StoreDecodeError
    { unStoreDecodeError :: String
    } deriving ( Eq, Ord, Show, Typeable )

  -- | An error type when casting.
  --
  -- @since 0.1.0.0
  data StoreTypeCastError = StoreTypeCastError
    { storeExpected :: !TypeRep
    , storeActual :: !TypeRep
    } deriving ( Eq, Ord, Show, Typeable )

  -- | A monad for handling a store. This has two functions corresponding to
  -- save/load.
  --
  -- 'loadDelay' delays loading values. Checking for the existence of a value
  -- and loading it have different costs.
  --
  -- @since 0.1.0.0
  class Monad m => MonadStore m where
    -- | Save a value with a key.
    save :: StoreKey -> StoreValue -> m ()
    -- | Load a value lazily.
    loadDelay :: StoreKey -> m (Maybe (StoreLoad m))

  -- | Load a value. The result branches in two points. The points is whether
  -- a value exists and if the type match.
  --
  -- @since 0.1.0.0
  load
    :: (MonadStore m, Binary a, Typeable a)
    => StoreKey
    -> m (Maybe (Either StoreError a))
  load sk = do
    msl <- loadDelay sk
    case msl of
      Nothing -> pure Nothing
      Just sl -> Just <$> runStoreLoad sl

  -- | Check if a value exists.
  --
  -- @since 0.1.0.0
  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    msl <- loadDelay sk
    return $ isJust msl
