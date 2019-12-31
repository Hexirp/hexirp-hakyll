module Hexyll.Core.Identifier.Pattern where

  import Prelude

  import Data.String (IsString (..))
  import Data.Binary (Binary (..), putWord8, getWord8)

  import qualified System.FilePath.Glob as Glob

  import Text.Regex.TDFA ((=~))

  import           Hexyll.Core.Identifier
  import qualified Hexyll.Core.Identifier.OldPattern as Old

  data PrimPattern
    = Glob Glob.Pattern
    | Regex String
    | Version (Maybe String)
    deriving (Eq, Show)

  instance IsString PrimPattern where
    fromString = Glob . fromString

  instance Binary PrimPattern where
    put x = case x of
      Glob p -> do
        putWord8 0
        put $ Glob.decompile p
      Regex r -> do
        putWord8 1
        put r
      Version v -> do
        putWord8 2
        put v
    get = do
      t <- getWord8
      case t of
        0 -> do
          s <- get
          return $ Glob $ Glob.compile s
        1 -> do
          r <- get
          return $ Regex r
        2 -> do
          v <- get
          return $ Version v
        _ -> error "Data.Binary.get: Invalid PrimPattern"

  newtype PatternData = PatternData { patternData :: [PrimPattern] }
    deriving (Eq, Show)

  instance IsString PatternData where
    fromString = fromPrim . fromString

  instance Binary PatternData where
    put x = put $ patternData x
    get = do
      x <- get
      return $ PatternData x

  instance Semigroup PatternData where
    PatternData x <> PatternData y = PatternData (x ++ y)

  instance Monoid PatternData where
    mempty = PatternData []

  newtype Pattern = Pattern { runPattern :: Identifier -> Bool }

  instance IsString Pattern where
    fromString = compile . fromString

  instance Semigroup Pattern where
    (<>) = (.&&.)

  instance Monoid Pattern where
    mempty = everything

  fromPrim :: PrimPattern -> PatternData
  fromPrim p = PatternData [p]

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

  match :: Pattern -> Identifier -> Bool
  match = runPattern

  fromPredicate :: (Identifier -> Bool) -> Pattern
  fromPredicate = Pattern

  fromIdentifier :: Identifier -> Pattern
  fromIdentifier i = Pattern (i ==)

  fromGlob :: String -> Pattern
  fromGlob = compilePrim . Glob . Glob.compile

  fromList :: [Identifier] -> Pattern
  fromList = foldr (\i p -> fromIdentifier i .||. p) nothing
  -- It's fused from @foldr (.||.) nothing . map fromIdentifier@.

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
