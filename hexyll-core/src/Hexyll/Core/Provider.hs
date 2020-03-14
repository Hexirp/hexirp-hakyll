module Hexyll.Core.Provider where

  import Prelude

  import Data.Time ( UTCTime (..) )

  import Hexyll.Core.Store

  import Path

  data ModificationTime = ModificationTime
    { modificationTime    :: Maybe UTCTime
    , modificationTimeOld :: Maybe UTCTime
    }

  class MonadStore m => MonadProvider m where
    getAllPath :: m [Path Rel File]
    getModificationTime :: Path Rel File -> m (Maybe ModificationTime)
