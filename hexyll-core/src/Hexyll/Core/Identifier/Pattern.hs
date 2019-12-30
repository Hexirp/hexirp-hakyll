module Hexyll.Core.Identifier.Pattern where

  import Prelude

  import qualified System.FilePath.Glob as Glob

  import Text.Regex.TDFA ((=~))

  import Hexyll.Core.Identifier

  data PrimPattern
    = Glob Glob.Pattern
    | Regex String
    | Version (Maybe String)
    deriving (Eq, Show)

  newtype Pattern = Pattern { runPattern :: Identifier -> Bool }

  compile :: PrimPattern -> Pattern
  compile (Glob p) = \i -> Glob.match p (toFilePath i)
  compile (Regex r) = \i -> toFilePath i =~ r
  compile (Version v) = \i -> getIdentVersion i == v

  everything :: Pattern
  everything = Pattern $ \_ -> True

  nothing :: Pattern
  nothing = Pattern $ \_ -> False

  fromGlob :: String -> Pattern
  fromGlob = compile . Glob . Glob.compile

  fromList :: [Identifier] -> Pattern
  fromList = foldr (.||.) nothing

  fromRegex :: String -> Pattern
  fromRegex = compile . Regex

  fromVersion :: Maybe String -> Pattern
  fromVersion = compile . Version

  hasVersion :: String -> Pattern
  hasVersion = fromVersion . Just

  hasNoVersion :: Pattern
  hasNoVersion = fromVersion Nothing

  (.&&.) :: Pattern -> Pattern -> Pattern
  Pattern f .&&. Pattern g = Pattern $ \i -> f i && g i

  (.||.) :: Pattern -> Pattern -> Pattern
  Pattern f .||. Pattern g = Pattern $ \i -> f i || g i

  complement :: Pattern -> Pattern
  complement (Pattern f) = Pattern $ \i -> not $ f i
