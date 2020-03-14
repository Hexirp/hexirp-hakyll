module Hexyll.Core.Provider where

  import Prelude

  import Data.Maybe ( isJust )

  import Data.Time ( UTCTime (..) )

  import qualified Data.ByteString as B

  import Hexyll.Core.Store

  import Path

  data ModificationTime = ModificationTime
    { modificationTime    :: UTCTime
    , modificationTimeOld :: Maybe UTCTime
    }

  newtype Body = Body { unBody :: B.ByteString }

  newtype ProviderLoad m a = ProviderLoad
    { runProviderLoad :: m a
    }

  class MonadStore m => MonadProvider m where

    getAllPath :: m [Path Rel File]

    countAllPath :: m Int
    countAllPath = length <$> getAllPath

    getModificationTimeDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))

    getBodyDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m Body))

  getModificationTime
    :: MonadProvider m => Path Rel File -> m (Maybe ModificationTime)
  getModificationTime path = do
    ml <- getModificationTimeDelay path
    case ml of
      Nothing -> pure Nothing
      Just l -> Just <$> runProviderLoad l

  getBody :: MonadProvider m => Path Rel File -> m (Maybe Body)
  getBody path = do
    ml <- getBodyDelay path
    case ml of
      Nothing -> pure Nothing
      Just l -> Just <$> runProviderLoad l

  isExistent :: MonadProvider m => Path Rel File -> m Bool
  isExistent path = do
    ml <- getBodyDelay path
    return $ isJust ml

  isModified :: MonadProvider m => Path Rel File -> m (Maybe Bool)
  isModified path = do
    mt <- getModificationTime path
    case mt of
      Nothing -> pure Nothing
      Just t -> pure (Just $ isModifiedTime t)

  isModifiedTime :: ModificationTime -> Bool
  isModifiedTime (ModificationTime tn mto) = case mto of
    Nothing -> False
    Just to -> tn > to
