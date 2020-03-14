module Hexyll.Core.Provider where

  import Prelude

  import Hexyll.Core.Identifier
  import Hexyll.Core.Metadata
  import Hexyll.Core.Store

  class (MonadMetadata m, MonadStore m) => MonadProvider m where
    getBody :: Identifier -> m String
