module Hexyll.Core.Provider where

  import Prelude

  import Data.Time ( UTCTime (..) )

  import qualified Data.ByteString as B

  import Hexyll.Core.Store

  import Path

  data ModificationTime = ModificationTime
    { modificationTime    :: Maybe UTCTime
    , modificationTimeOld :: Maybe UTCTime
    }

  newtype Body = Body { unBody :: B.ByteString }

  newtype ProviderLoad m a = ProviderLoad
    { runProviderLoad :: m a
    }

  class MonadStore m => MonadProvider m where

    getAllPath :: m [Path Rel File]

    getModificationTimeDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))

    getBodyDelay
      :: Path Rel File -> m (Maybe (ProviderLoad m Body))

    getModificationTime :: Path Rel File -> m (Maybe ModificationTime)

    getBody :: Path Rel File -> m (Maybe Body)

    isExistent :: Path Rel File -> m Bool

    isModified :: Path Rel File -> m (Maybe Bool)
