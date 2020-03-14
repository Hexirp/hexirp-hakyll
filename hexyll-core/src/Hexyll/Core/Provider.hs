module Hexyll.Core.Provider where

  import Prelude

  import Data.Time ( UTCTime (..) )

  import qualified Data.ByteString.Lazy as BL

  import Hexyll.Core.Store

  import Path

  data ModificationTime = ModificationTime
    { modificationTime    :: Maybe UTCTime
    , modificationTimeOld :: Maybe UTCTime
    }

  newtype Body = Body { unBody :: BL.ByteString }

  class MonadStore m => MonadProvider m where

    getAllPath :: m [Path Rel File]

    getModificationTime :: Path Rel File -> m (Maybe ModificationTime)

    getBody :: Path Rel File -> m (Maybe Body)

    isExistent :: Path Rel File -> m Bool

    isModified :: Path Rel File -> m (Maybe Bool)
