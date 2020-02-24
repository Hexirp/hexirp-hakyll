module Hexyll.Core.Log where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  type StoreKey = [String]

  data StoreResult a = StoreFound a | StoreNotFound | StoreWrongType TypeRep TypeRep
    deriving (Eq, Show, Typeable)

  data StoreEnv = StoreEnv
    { storeSet :: !(forall a. (Binary a, Typeable a) => StoreKey -> a -> IO ())
    , storeGet :: !(forall a. (Binary a, Typeable a) => StoreKey -> IO (StoreResult a))
    } deriving (Typeable)
