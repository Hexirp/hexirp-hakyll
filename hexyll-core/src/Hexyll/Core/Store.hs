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
    save :: Typeable a => StoreKey -> a -> m ()
    load :: Typeable a => StoreKey -> m (StoreResult a)
    remove :: StoreKey -> m Bool
