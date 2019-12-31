-- |
-- Module:      Hexyll.Core.Identifier
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Pattern', a type of pattern matching to 'Identifier'.
module Hexyll.Core.Identifier.Pattern
  ( -- * Types
    PrimPattern (..)
  , PattrnData (..)
  , Pattern (..)
    -- * Compiling
  , fromPrim
  , compileOld
  , compilePrim
  , compile
    -- * Matching
  , match
    -- * Creating Patterns
  , fromPredicate
  , fromIdentifier
  , fromGlob
  , fromList
  , fromRegex
  , fromVersion
  , hasVersion
  , hasNoVersion
    -- * Composing patterns
  , everything
  , nothing
  , (.&&.)
  , (.||.)
  , complement
  ) where

  import Prelude

  import Data.String (IsString (..))
  import Data.Binary (Binary (..), putWord8, getWord8)

  import qualified System.FilePath.Glob as Glob

  import Text.Regex.TDFA ((=~))

  import           Hexyll.Core.Identifier
  import qualified Hexyll.Core.Identifier.OldPattern as Old

  -- | A primitive token of 'PatternData'.
  --
  -- 'PrimPattern' has the instance of 'IsString' interpreting the string as
  -- a glob pattern.
  --
  -- @since 0.1.0.0
  data PrimPattern
    = Glob Glob.Pattern
    | Regex String
    | Version (Maybe String)
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString PrimPattern where
    fromString = Glob . fromString

  -- | @since 0.1.0.0
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

  -- | A serializable 'Pattern'.
  --
  -- 'PatternData' is based on logical conjuction, and is not contain "or"
  -- pattern.
  newtype PatternData = PatternData { patternData :: [PrimPattern] }
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString PatternData where
    fromString = fromPrim . fromString

  -- | @since 0.1.0.0
  instance Binary PatternData where
    put x = put $ patternData x
    get = do
      x <- get
      return $ PatternData x

  -- | @since 0.1.0.0
  instance Semigroup PatternData where
    PatternData x <> PatternData y = PatternData (x ++ y)

  -- | @since 0.1.0.0
  instance Monoid PatternData where
    mempty = PatternData []

  -- | A type of pattern matching to 'Identifier'.
  --
  -- The 'Monoid' instance is based on logical conjuction.
  --
  -- @since 0.1.0.0
  newtype Pattern = Pattern { runPattern :: Identifier -> Bool }

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString = compile . fromString

  -- | @since 0.1.0.0
  instance Semigroup Pattern where
    (<>) = (.&&.)

  -- | @since 0.1.0.0
  instance Monoid Pattern where
    mempty = everything

  -- | Convert a 'PrimPattern' to a 'PatternData'.
  --
  -- @since 0.1.0.0
  fromPrim :: PrimPattern -> PatternData
  fromPrim p = PatternData [p]

  -- | Compile a 'Old.Pattern' to a 'Pattern'.
  --
  -- @since 0.1.0.0
  compileOld :: Old.Pattern -> Pattern
  compileOld p = Pattern $ Old.matches p

  -- | Compile a 'PrimPattern' to a 'Pattern'.
  --
  -- @since 0.1.0.0
  compilePrim :: PrimPattern -> Pattern
  compilePrim (Glob p)    = Pattern $ \i -> Glob.match p (toFilePath i)
  compilePrim (Regex r)   = Pattern $ \i -> toFilePath i =~ r
  compilePrim (Version v) = Pattern $ \i -> getIdentVersion i == v

  -- | Compile a 'PatternData' to a 'Pattern'.
  compile :: PatternData -> Pattern
  compile (PatternData x) = foldr (\p s -> compilePrim p .&&. s) everything x
  -- It's fused from @foldr (.&&.) everything . map compilePrim@.

  -- | A pattern that matches everything.
  --
  -- @since 0.1.0.0
  everything :: Pattern
  everything = Pattern $ \_ -> True

  -- | A pattern that matches nothing.
  --
  -- @since 0.1.0.0
  nothing :: Pattern
  nothing = Pattern $ \_ -> False

  -- | Match a pattern to an identifier.
  --
  -- @since 0.1.0.0
  match :: Pattern -> Identifier -> Bool
  match = runPattern

  -- | Make a pattern from a predicate.
  --
  -- @since 0.1.0.0
  fromPredicate :: (Identifier -> Bool) -> Pattern
  fromPredicate = Pattern

  -- | Make a pattern from a identifier. @fromIdentifier i@ is equal to
  -- @fromPredicate (i ==)@.
  --
  -- @since 0.1.0.0
  fromIdentifier :: Identifier -> Pattern
  fromIdentifier i = Pattern (i ==)

  -- | Make a pattern from a glob pattern. See "System.FilePath.Glob".
  --
  -- @since 0.1.0.0
  fromGlob :: String -> Pattern
  fromGlob = compilePrim . Glob . Glob.compile

  -- | Make a pattern from a identifier list. @fromList@ is equal to @foldr
  -- (.||.) nothing . map fromIdentifier@.
  --
  -- @since 0.1.0.0
  fromList :: [Identifier] -> Pattern
  fromList = foldr (\i p -> fromIdentifier i .||. p) nothing
  -- It's fused from @foldr (.||.) nothing . map fromIdentifier@.

  -- | Make a pattern from a regex pattern. See "Text.Regex.TDFA".
  --
  -- @since 0.1.0.0
  fromRegex :: String -> Pattern
  fromRegex = compilePrim . Regex

  -- | Make a pattern from a version. It is a pattern matching by strict
  -- equality.
  --
  -- @since 0.1.0.0
  fromVersion :: Maybe String -> Pattern
  fromVersion = compilePrim . Version

  -- | Make a pattern from a existing version. @hasVersion@ is equal to
  -- @fromVersion . Just@.
  --
  -- @since 0.1.0.0
  hasVersion :: String -> Pattern
  hasVersion = fromVersion . Just

  -- | Make a pattern from no version. @hasVersion@ is equal to @fromVersion
  -- Nothing@.
  --
  -- @since 0.1.0.0
  hasNoVersion :: Pattern
  hasNoVersion = fromVersion Nothing

  -- | Make an "and" pattern.
  --
  -- @since 0.1.0.0
  (.&&.) :: Pattern -> Pattern -> Pattern
  Pattern f .&&. Pattern g = Pattern $ \i -> f i && g i

  -- | Make an "or" pattern.
  --
  -- @since 0.1.0.0
  (.||.) :: Pattern -> Pattern -> Pattern
  Pattern f .||. Pattern g = Pattern $ \i -> f i || g i

  -- | Make an compelent pattern.
  --
  -- @since 0.1.0.0
  complement :: Pattern -> Pattern
  complement (Pattern f) = Pattern $ \i -> not $ f i
