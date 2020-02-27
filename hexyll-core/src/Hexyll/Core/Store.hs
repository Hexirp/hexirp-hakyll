module Hexyll.Core.Store where

  import Prelude

  import Data.Typeable ( Typeable, TypeRep )

  type StoreKey = String

  data StoreValue where
    StoreValue :: (Binary a, Typeable a) => a -> StoreValue

  data StoreResult a
    = StoreFound a
    | StoreNotFound
    | StoreWrongType TypeRep
    deriving (Eq, Show, Typeable)

  class Monad m => MonadStore m where
    save :: StoreKey -> StoreValue -> m ()
    load :: StoreKey -> m (Maybe StoreValue)
