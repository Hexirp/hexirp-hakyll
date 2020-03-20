module Hexyll.Core.Routes where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Path
  import qualified Path.Hexyll

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
  setExtension ext = Routes $ \i -> (: []) $
    case Path.Hexyll.replaceExtension ext $ fromIdentifierToPath i of
      Left e -> error $ unwords
        [ "setExtension: "
        , "Something wrong happened."
        , "replaceExtension threw an error:"
        , show (show e)
        ]
      Right p -> p

  matchRoute :: Pattern -> Routes -> Routes
  matchRoute pat (Routes f) = Routes $ \i ->
    if match i pat then f i else []
