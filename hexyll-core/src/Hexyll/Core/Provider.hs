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

  import Data.Typeable   ( Typeable )
  import Control.DeepSeq ( NFData (..) )

  import Data.Maybe ( isJust )

  import qualified Data.Set as S

  import Data.Time ( UTCTime (..) )

  import qualified Data.ByteString.Lazy as BL

  import Hexyll.Core.Store

  import Path

  -- | A path of a resource.
  --
  -- @since 0.1.0.0
  data Resource = Resource { unResource :: Path Rel File }
    deriving ( Eq, Old, Show, Typeable )

  -- | @since 0.1.0.0
  instance NFData Resource where
    rnf (Resource x) = rnf x

  -- | @since 0.1.0.0
  instance Binary Resource where
    put (Resource x) = put (toFilePath x)
    get = do
      x' <- get
      case parseRelFile x' of
        Nothing -> error "Data.Binary.get: Invalid Resource"
        Just x -> return x

  -- | A type of modification time.
  --
  -- If 'modificationTimeOld' is 'Nothing', it means that the file did not
  -- exist in the previous run.
  --
  -- @since 0.1.0.0
  data ModificationTime = ModificationTime
    { modificationTime    :: !UTCTime
    , modificationTimeOld :: !(Maybe UTCTime)
    } deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance NFData ModificationTime where
    rnf (ModificationTime tn mto) = rnf tn `seq` rnf mto `seq` ()

  -- | Check if a 'ModificationTime' means that the file is modified.
  --
  -- If 'modificationTimeOld' is 'Nothing', it means that the file did not
  -- exist in the previous run. Then this function returns 'True'.
  --
  -- If 'modificationTime' is greater than 'modificationTimeOld', it means
  -- the file is newer than the previous run. Then this function returns
  -- 'True'.
  --
  -- @since 0.1.0.0
  isProofOldness :: ModificationTime -> Bool
  isProofOldness (ModificationTime tn mto) = case mto of
    Nothing -> False
    Just to -> tn > to

  -- | A body of a file.
  --
  -- @since 0.1.0.0
  newtype Body = Body { unBody :: BL.ByteString }
    deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance NFData Body where
    rnf (Body x) = rnf x

  -- | A delayed loading effect.
  --
  -- @since 0.1.0.0
  newtype ProviderLoad m a = ProviderLoad
    { runProviderLoad :: m a
    } deriving ( Eq, Ord, Show, Typeable )

  -- | Map over 'ProviderLoad'.
  --
  -- @since 0.1.0.0
  mapProviderLoad :: (m a -> n b) -> ProviderLoad m a -> ProviderLoad n b
  mapProviderLoad f (ProviderLoad pl) = ProviderLoad (f pl)

  -- | Unwrap 'ProviderLoad' and evaluate each action in the structure
  -- from left to right, and collect the results.
  --
  -- @since 0.1.0.0
  sequenceProviderLoad
    :: (Traversable t, Applicative m) => t (ProviderLoad m a) -> m (t a)
  sequenceProviderLoad = traverse runProviderLoad

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
    getAllPath :: m (S.Set Resource)

    -- | Count all paths.
    --
    -- @since 0.1.0.0
    countAllPath :: m Int
    countAllPath = S.size <$> getAllPath

    -- | Get the modification time of a file lazily.
    --
    -- @since 0.1.0.0
    getModificationTimeDelay
      :: Resource -> m (Maybe (ProviderLoad m ModificationTime))

    -- | Get the body of a file lazily.
    --
    -- @since 0.1.0.0
    getBodyDelay
      :: Resource -> m (Maybe (ProviderLoad m Body))

  -- | Get the modification time of a file.
  --
  -- @since 0.1.0.0
  getModificationTime
    :: MonadProvider m => Resource -> m (Maybe ModificationTime)
  getModificationTime path = do
    ml <- getModificationTimeDelay path
    sequenceProviderLoad ml

  -- | Get the body of a file.
  --
  -- @since 0.1.0.0
  getBody :: MonadProvider m => Resource -> m (Maybe Body)
  getBody path = do
    ml <- getBodyDelay path
    sequenceProviderLoad ml

  -- | Check if a file exists.
  --
  -- @since 0.1.0.0
  isExistent :: MonadProvider m => Resource -> m Bool
  isExistent path = do
    ml <- getBodyDelay path
    return $ isJust ml

  -- | Check if a file is modified -- a file is newer than the previous run.
  --
  -- @since 0.1.0.0
  isModified :: MonadProvider m => Resource -> m (Maybe Bool)
  isModified path = do
    mt <- getModificationTime path
    return $ fmap isProofOldness mt
