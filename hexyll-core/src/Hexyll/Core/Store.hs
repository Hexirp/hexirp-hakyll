module Hexyll.Core.Store where

  import Prelude

  import Data.Typeable ( Typeable, TypeRep )

  type StoreKey = String

  data StoreResult a
    = StoreFound a
    | StoreNotFound
    | StoreWrongType TypeRep
    deriving (Eq, Show, Typeable)

  class Monad m => MonadStore m where
    setInStore :: Typeable a => StoreKey -> a -> m ()
    getInStore :: Typeable a => StoreKey -> m (StoreResult a)
    removeInStore :: StoreKey -> m Bool
