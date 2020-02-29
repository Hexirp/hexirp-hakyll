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
  import Data.Proxy ( Proxy (..), asProxyTypeOf )

  import Data.Typeable ( Typeable, typeOf, typeRep, cast, TypeRep )
  import Data.Binary   ( Binary (..), Put, Get )

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

  -- | Put a 'StoreValue' rawly. It is the same as 'putStVal', but uses a bare
  -- value.
  --
  -- @since 0.1.0.0
  putStValRawly :: (Binary a, Typeable a) => a -> Put
  putStValRawly x = do
    put (typeOf x)
    put x

  -- | Put a 'StoreValue'. However, there is no correct @getStoreValue@
  -- corresponding to 'putStVal'.
  --
  -- @since 0.1.0.0
  putStVal :: StoreValue -> Put
  putStVal (MkStoreValue x) = do
    put (typeOf x)
    put x

  -- | Get a 'StoreValue'. Due to technical limitations you have to specify
  -- the type. If the binary is not of the specified type, return an error.
  --
  -- @since 0.1.0.0
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

  -- | Get a 'StoreValue' and unwrap the value. This also has the same problem
  -- as 'getStValRestrictly'.
  --
  -- 'getStValRestrictly' wraps the value you get. 'getUnwrapStVal' returns it
  -- as is.
  --
  -- @since 0.1.0.0
  getUnwrapStVal
    :: (Binary a, Typeable a) => Proxy a -> Get (Either StoreError a)
  getUnwrapStVal proxy = let trExpect = typeRep proxy in do
    trActual <- get
    if trActual == trExpect
      then do
        x <- get
        return $ Right x `const` (x `asProxyTypeOf` proxy)
      else
        return $ Left (StoreError trExpect trActual)

  -- | A type of delayed loading.
  --
  -- @since 0.1.0.0
  newtype StoreLoad m = StoreLoad
    { runStoreLoad
        :: forall a. (Binary a, Typeable a) => m (Either StoreError a)
    } deriving ( Typeable )

  -- | An error type when casting.
  --
  -- @since 0.1.0.0
  data StoreError = StoreError
    { storeExpect :: TypeRep
    , storeActual :: TypeRep
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
