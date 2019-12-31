module Hexyll.Core.Identifier.Pattern where

  import Prelude

  import qualified System.FilePath.Glob as Glob

  import Text.Regex.TDFA ((=~))

  import Hexyll.Core.Identifier

  import qualified Hexyll.Core.Identifier.OldPattern as Old

  data PrimPattern
    = Glob Glob.Pattern
    | Regex String
    | Version (Maybe String)
    deriving (Eq, Show)

  newtype PatternData = PatternData { patternData :: [PrimPattern] }
    deriving (Eq, Show)

  newtype Pattern = Pattern { runPattern :: Identifier -> Bool }

  compileOld :: Old.Pattern -> Pattern
  compileOld p = Pattern $ Old.matches p

  compilePrim :: PrimPattern -> Pattern
  compilePrim (Glob p)    = Pattern $ \i -> Glob.match p (toFilePath i)
  compilePrim (Regex r)   = Pattern $ \i -> toFilePath i =~ r
  compilePrim (Version v) = Pattern $ \i -> getIdentVersion i == v

  compile :: PatternData -> Pattern
  compile (PatternData x) = foldr (\p s -> compilePrim p .&&. s) everything x
  -- It's fused from @foldr (.&&.) everything . map compilePrim@.

  everything :: Pattern
  everything = Pattern $ \_ -> True

  nothing :: Pattern
  nothing = Pattern $ \_ -> False

  fromIdentifier :: Identifier -> Pattern
  fromIdentifier i = Pattern (i ==)

  fromGlob :: String -> Pattern
  fromGlob = compilePrim . Glob . Glob.compile

  fromList :: [Identifier] -> Pattern
  fromList = foldr (\i p -> fromIdentifier i .||. p) nothing

  fromRegex :: String -> Pattern
  fromRegex = compilePrim . Regex

  fromVersion :: Maybe String -> Pattern
  fromVersion = compilePrim . Version

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
