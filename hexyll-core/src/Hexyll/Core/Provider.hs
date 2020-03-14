module Hexyll.Core.Provider where

  import Prelude

  import Hexyll.Core.Identifier
  import Hexyll.Core.Metadata

  class MonadMetadata m => MonadProvider m where
    getBody :: Identifier -> m String
