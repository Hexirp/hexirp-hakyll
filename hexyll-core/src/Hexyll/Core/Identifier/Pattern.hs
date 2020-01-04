module Hexyll.Core.Identifier.Pattern where

  import Prelude

  import Data.String (IsString (..))
  import Data.Binary (Binary (..), putWord8, getWord8)

  import qualified System.FilePath.Glob as Glob

  import Text.Regex.TDFA ((=~))

  import Hexyll.Core.Identifier

  -- | A primitive token of 'PatternExpr'.
  --
  -- The type allows three patterns -- glob pattern, regex pattern, and
  -- matching version.
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

  -- | Match a 'Identifier' with 'PrimPattern'
  --
  -- @since 0.1.0.0
  matchPrim :: Identifier -> PrimPattern -> Bool
  matchPrim i x = case x of
    Glob p -> Glob.match p (toFilePath i)
    Regex r -> toFilePath i =~ r
    Version mv -> getIdentVersion i == mv

  -- | A expression of pattern matching to 'Identifier', reprensented as
  -- @'Identifier' -> 'Bool'@.
  --
  -- * 'fromPrim' - from a 'PrimPattern'.
  -- * 'fromGlob' - from a glob pattern. The function checks whether it is a
  --   correct glob pattern.
  -- * 'fromRegex' - from a regex pattern.
  -- * 'fromVersion' - from a version of 'Identifier'. The pattern is
  --   interpreted as: @matchExpr (fromVersion mv) i === getIdentVersion i ==
  --   mv@.
  -- * 'everything' - The pattern matches everything.
  -- * @(.&&.)@ - The logical conjunction of two patterns.
  -- * 'nothing' - The patter matches nothing.
  -- * @(.||.)@ - The logical disjunction of two patterns.
  -- * 'complement' - The logical complement of a pattern.
  --
  -- @since 0.1.0.0
  data PatternExpr
    = PePrim PrimPattern
    | PeEverything
    | PeAnd PatternExpr PatternExpr
    | PeNothing
    | PeOr PatternExpr PatternExpr
    | PeComplement PatternExpr
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString PatternExpr where
    fromString = fromPrim . fromString

  -- | @since 0.1.0.0
  instance Binary PatternExpr where
    put x = case x of
      PePrim pp -> do
        putWord8 0
        put pp
      PeEverything ->
        putWord8 1
      PeAnd x0 x1 -> do
        putWord8 2
        put x0
        put x1
      PeNothing ->
        putWord8 3
      PeOr x0 x1 -> do
        putWord8 4
        put x0
        put x1
      PeComplement xc -> do
        putWord8 5
        put xc
    get = do
      t <- getWord8
      case t of
        0 -> do
          pp <- get
          return $ PePrim pp
        1 ->
          return $ PeEverything
        2 -> do
          x0 <- get
          x1 <- get
          return $ PeAnd x0 x1
        3 ->
          return $ PeNothing
        4 -> do
          x0 <- get
          x1 <- get
          return $ PeOr x0 x1
        5 -> do
          xc <- get
          return $ PeComplement xc
        _ -> error "Data.Binary.get: Invalid PatternExpr"

  -- | Make a pattern from a 'PrimPattern'.
  --
  -- @since 0.1.0.0
  fromPrim :: PrimPattern -> PatternExpr
  fromPrim = PePrim

  -- | Make a pattern from a glob pattern.
  --
  -- The function checks whether it is a correct glob pattern.
  --
  -- @since 0.1.0.0
  fromGlob :: String -> PatternExpr
  fromGlob = fromPrim . Glob . Glob.compile

  -- | Make a pattern from a regex pattern.
  --
  -- @since 0.1.0.0
  fromRegex :: String -> PatternExpr
  fromRegex = fromPrim . Regex

  -- | Make a pattern from a version.
  --
  -- The pattern is interpreted as: @matchExpr (fromVersion mv) i ===
  -- getIdentVersion i == mv@.
  --
  -- @since 0.1.0.0
  fromVersion :: Maybe String -> PatternExpr
  fromVersion = fromPrim . Version

  -- | The pattern matches everything.
  --
  -- @since 0.1.0.0
  everything :: PatternExpr
  everything = PeEverything

  -- | The logical conjunction of two patterns.
  --
  -- @since 0.1.0.0
  (.&&.) :: PatternExpr -> PatternExpr -> PatternExpr
  (.&&.) = PeAnd

  -- | The pattern matches nothing.
  --
  -- @since 0.1.0.0
  nothing :: PatternExpr
  nothing = PeNothing

  -- | The logical disjunction of two patterns.
  --
  -- @since 0.1.0.0
  (.||.) :: PatternExpr -> PatternExpr -> PatternExpr
  (.||.) = PeOr

  -- | The logical complement of a pattern.
  --
  -- @since 0.1.0.0
  complement :: PatternExpr -> PatternExpr
  complement = PeComplement

  -- | Convert a 'PatternExpr' to a 'PatternConj'.
  --
  -- @since 0.1.0.0
  toPatternConj :: PatternExpr -> PatternConj
  toPatternConj p = PatternConj [p]

  -- | Convert a 'PatternExpr' to a 'PatternDisj'.
  --
  -- @since 0.1.0.0
  toPatternDisj :: PatternExpr -> PatternDisj
  toPatternDisj p = PatternDisj [p]

  -- | Match a 'Identifier' with a 'PatternExpr'.
  --
  -- @since 0.1.0.0
  matchExpr :: Identifier -> PatternExpr -> Bool
  matchExpr i x = case x of
    PePrim p -> matchPrim i p
    PeEverything -> True
    PeAnd x0 x1 -> matchExpr i x0 && matchExpr i x1
    PeNothing -> False
    PeOr x0 x1 -> matchExpr i x0 || matchExpr i x1
    PeComplement xc -> not (matchExpr i xc)

  -- | A conjunction of 'PatternExpr's.
  --
  -- 'PatternConj' has the instance of 'Monoid' that implements 'mappend' as
  -- logical conjunction.
  --
  -- @since 0.1.0.0
  newtype PatternConj = PatternConj { unPatternConj :: [PatternExpr] }
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString PatternConj where
    fromString = toPatternConj . fromString

  -- | @since 0.1.0.0
  instance Binary PatternConj where
    put (PatternConj x) = put x
    get = PatternConj <$> get

  -- | @since 0.1.0.0
  instance Semigroup PatternConj where
    PatternConj x <> PatternConj y = PatternConj (x <> y)

  -- | @since 0.1.0.0
  instance Monoid PatternConj where
    mempty = PatternConj mempty

  -- | Match a 'Identifier' with a 'PatternConj'.
  --
  -- @since 0.1.0.0
  matchConj :: Identifier -> PatternConj -> Bool
  matchConj i (PatternConj x) = all (matchExpr i) x

  -- | A disjunction of 'PatternExpr's.
  --
  -- 'PatternDisj' has the instance of 'Monoid' that implements 'mappend' as
  -- logical disjunction.
  --
  -- @since 0.1.0.0
  newtype PatternDisj = PatternDisj { unPatternDisj :: [PatternExpr] }
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString PatternDisj where
    fromString = toPatternDisj . fromString

  -- | @since 0.1.0.0
  instance Binary PatternDisj where
    put (PatternDisj x) = put x
    get = PatternDisj <$> get

  -- | @since 0.1.0.0
  instance Semigroup PatternDisj where
    PatternDisj x <> PatternDisj y = PatternDisj (x <> y)

  -- | @since 0.1.0.0
  instance Monoid PatternDisj where
    mempty = PatternDisj mempty

  -- | Match a 'Identifier' with a 'PatternDisj'.
  --
  -- @since 0.1.0.0
  matchDisj :: Identifier -> PatternDisj -> Bool
  matchDisj i (PatternDisj x) = any (matchExpr i) x

  -- | A type of pattern matching to 'Identifier'.
  --
  -- @since 0.1.0.0
  newtype Pattern = Pattern { runPattern :: Identifier -> Bool }

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString = compileExpr . fromString

  -- | Compile a 'PatternExpr' to a 'Pattern'.
  --
  -- @since 0.1.0.0
  compileExpr :: PatternExpr -> Pattern
  compileExpr p = Pattern (\i -> matchExpr i p)

  -- | Compile a 'PatternConj' to a 'Pattern'.
  --
  -- @since 0.1.0.0
  compileConj :: PatternConj -> Pattern
  compileConj p = Pattern (\i -> matchConj i p)

  -- | Compile a 'PatternDisj' to a 'Pattern'.
  --
  -- @since 0.1.0.0
  compileDisj :: PatternDisj -> Pattern
  compileDisj p = Pattern (\i -> matchDisj i p)

  -- | Match a 'Identifier' with a 'Pattern'.
  --
  -- @since 0.1.0.0
  match :: Identifier -> Pattern -> Bool
  match i (Pattern p) = p i
