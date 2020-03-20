module Hexyll.Core.Routes where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Path

  import Hexyll.Core.Identifier

  newtype Routes = Routes { unRoutes :: Identifier -> [Path Rel File] }
    deriving ( Typeable )

  instance NFData Routes where
    rnf (Routes x) = rnf x

  instance Semigroup Routes where
    Routes f <> Routes g = Routes (\i -> f i ++ g i)

  instance Monoid Routes where
    mempty = Routes (\_ -> [])
