module HExyll.Core.Identifier.PatternExpr where

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

  data PatternExpr
    = PePrim PrimPattern
    | PeEverything
    | PeAnd PatternExpr PatternExpr
    | PeNothing
    | PeOr PatternExpr PatternExpr
    | PeComplement PatternExpr
    deriving (Eq, Show)

  instance IsString PatternExpr where
    fromString = fromPrim . fromString

  fromPrim :: PrimPattern -> PatternExpr
  fromPrim = PePrim