module Hexyll.Core.ProviderEnv where

  import Prelude

  import qualified Data.Set as S

  import Path

  import Hexyll.Core.Provider

  data ProviderEnv = ProviderEnv
    { providerGetAllPath :: IO (S.Set (Path Rel File))
    , providerGetModificationTimeDelay
        :: Path Rel File -> IO (Maybe (ProviderLoad IO ModificationTime))
    , providerGetBodyDelay
        :: Path Rel File -> IO (Maybe (ProviderLoad IO Body))
    }
