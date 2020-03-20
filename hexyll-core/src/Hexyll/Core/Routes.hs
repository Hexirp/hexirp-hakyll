module Hexyll.Core.Routes where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Path
  import System.FilePath ( replaceExtension )

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern

  newtype Routes = Routes { unRoutes :: Identifier -> [Path Rel File] }
    deriving ( Typeable )

  instance NFData Routes where
    rnf (Routes x) = rnf x

  instance Semigroup Routes where
    Routes f <> Routes g = Routes (\i -> f i ++ g i)

  instance Monoid Routes where
    mempty = Routes (\_ -> [])

  idRoute :: Routes
  idRoute = Routes $ \i -> [fromIdentifierToPath i]

  setExtension :: String -> Routes
  setExtension ext = Routes $ \i -> parseRelFile $ replaceExtension ext $ fromIdentifierToFilePath i
  -- setExtension ext = Routes $ \i -> replaceExtension ext (toPath i)

  matchRoute :: Pattern -> Routes -> Routes
  matchRoute pat (Routes f) = Routes $ \i ->
    if match i pat then f i else []
