{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.Provider
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides the basic types and type classes for handling a
-- provider. A provider manages files under the provider directory.
--
-- @since 0.1.0.0
module Hexyll.Core.Provider where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Time ( UTCTime (..) )

  import qualified Data.ByteString as B

  import Hexyll.Core.Store

  import Path

  -- | A type of modification time.
  --
  -- If 'modificationTimeOld' is 'Nothing', it means that the file did not
  -- exist in the previous run.
  --
  -- If 'modificationTime' is greater than 'modificationTimeOld', it means
  -- the file is newer than the previous run.
  --
  -- @since 0.1.0.0
  data ModificationTime = ModificationTime
    { modificationTime    :: UTCTime
    , modificationTimeOld :: Maybe UTCTime
    }

  -- | Check if a 'ModificationTime' means that the file is modified.
  --
  -- If 'modificationTimeOld' is 'Nothing', it means that the file did not
  -- exist in the previous run.
  --
  -- If 'modificationTime' is greater than 'modificationTimeOld', it means
  -- the file is newer than the previous run.
  isModifiedTime :: ModificationTime -> Bool
  isModifiedTime (ModificationTime tn mto) = case mto of
    Nothing -> False
    Just to -> tn > to

  -- | A body of a file.
  --
  -- @since 0.1.0.0
  newtype Body = Body { unBody :: B.ByteString }

  -- | A delayed loading effect.
  --
  -- @since 0.1.0.0
  newtype ProviderLoad m a = ProviderLoad
    { runProviderLoad :: m a
    }

  -- | A monad for handling a provider. It inherits 'MonadStore' for caching.
  --
  -- Most functions in 'MonadProvider' return 'Nothing' because the file does
  -- not exist.
  --
  -- @since 0.1.0.0
  class MonadStore m => MonadProvider m where

    -- | Get all paths.
    --
    -- @since 0.1.0.0
    getAllPath :: m [Path Rel File]

    -- | Count all paths.
    --
    -- @since 0.1.0.0
    countAllPath :: m Int
    countAllPath = length <$> getAllPath

    -- | Get the modification time of a file lazily.
    --
    -- @since 0.1.0.0
    getModificationTimeDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))


    -- | Get the body of a file lazily.
    --
    -- @since 0.1.0.0
    getBodyDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m Body))

  -- | Get the modification time of a file.
  --
  -- @since 0.1.0.0
  getModificationTime
    :: MonadProvider m => Path Rel File -> m (Maybe ModificationTime)
  getModificationTime path = do
    ml <- getModificationTimeDelay path
    case ml of
      Nothing -> pure Nothing
      Just l -> Just <$> runProviderLoad l

  -- | Get the body of a file.
  --
  -- @since 0.1.0.0
  getBody :: MonadProvider m => Path Rel File -> m (Maybe Body)
  getBody path = do
    ml <- getBodyDelay path
    case ml of
      Nothing -> pure Nothing
      Just l -> Just <$> runProviderLoad l

  -- | Check if a file exists.
  --
  -- @since 0.1.0.0
  isExistent :: MonadProvider m => Path Rel File -> m Bool
  isExistent path = do
    ml <- getBodyDelay path
    return $ isJust ml

  -- | Check if a file is modified -- a file is newer than the previous run.
  --
  -- @since 0.1.0.0
  isModified :: MonadProvider m => Path Rel File -> m (Maybe Bool)
  isModified path = do
    mt <- getModificationTime path
    case mt of
      Nothing -> pure Nothing
      Just t -> pure (Just $ isModifiedTime t)
