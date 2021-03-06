--------------------------------------------------------------------------------
-- | Internally used compiler module
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Hexyll.Core.OldCompiler.Internal
    ( -- * Types
      Snapshot
    , CompilerRead (..)
    , CompilerWrite (..)
    , CompilerErrors (..)
    , CompilerResult (..)
    , Compiler (..)
    , runCompiler

      -- * Core operations
    , compilerResult
    , compilerTell
    , compilerAsk
    , compilerUnsafeIO

      -- * Error operations
    , compilerThrow
    , compilerNoResult
    , compilerCatch
    , compilerTry
    , compilerErrorMessages

      -- * Utilities
    , compilerDebugEntries
    , compilerTellDependenciesCache
    , compilerTellCacheHits

    , Pattern (..)
    , match
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            (Alternative (..))
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (forM_)
import           Control.Monad.Except           (MonadError (..))
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Set                       (Set)
import qualified Data.Set                       as S


--------------------------------------------------------------------------------
import           Hexyll.Core.Configuration
import           Hexyll.Core.Dependencies
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern hiding ( Pattern, match )
import qualified Hexyll.Core.Logger as Logger
import           Hexyll.Core.Metadata           hiding ( Pattern, unPattern )
import qualified Hexyll.Core.Metadata as Meta   ( Pattern (..) )
import           Hexyll.Core.OldProvider
import           Hexyll.Core.OldRoutes             hiding ( Pattern, unPattern, match )
import           Hexyll.Core.OldStore


import Data.Typeable ( Typeable )
import Data.String ( IsString (..) )

--------------------------------------------------------------------------------
-- | Whilst compiling an item, it possible to save multiple snapshots of it, and
-- not just the final result.
type Snapshot = String


--------------------------------------------------------------------------------
-- | Environment in which a compiler runs
data CompilerRead = CompilerRead
    { -- | Main configuration
      compilerConfig     :: Configuration
    , -- | Underlying identifier
      compilerUnderlying :: Identifier
    , -- | Resource provider
      compilerProvider   :: Provider
    , -- | List of all known identifiers
      compilerUniverse   :: Set Identifier
    , -- | Site routes
      compilerRoutes     :: Routes
    , -- | Compiler store
      compilerStore      :: Store
    , -- | Logger
      compilerLogger     :: Logger.Logger
    }


--------------------------------------------------------------------------------
data CompilerWrite = CompilerWrite
    { compilerDependencies :: [Dependency]
    , compilerCache        :: [Identifier]
    , compilerCacheHits    :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup CompilerWrite where
    (<>) (CompilerWrite d1 i1 h1) (CompilerWrite d2 i2 h2) =
        CompilerWrite (d1 ++ d2) (i1 ++ i2) (h1 + h2)

instance Monoid CompilerWrite where
    mempty  = CompilerWrite [] [] 0
    mappend = (<>)


--------------------------------------------------------------------------------
-- | Distinguishes reasons in a 'CompilerError'
data CompilerErrors a
    -- | One or more exceptions occured during compilation
    = CompilationFailure (NonEmpty a)
    -- | Absence of any result, most notably in template contexts.  May still
    -- have error messages.
    | CompilationNoResult [a]
    deriving Functor


-- | Unwrap a `CompilerErrors`
compilerErrorMessages :: CompilerErrors a -> [a]
compilerErrorMessages (CompilationFailure x)  = NonEmpty.toList x
compilerErrorMessages (CompilationNoResult x) = x


--------------------------------------------------------------------------------
-- | An intermediate result of a compilation step
data CompilerResult a
    = CompilerDone a CompilerWrite
    | CompilerSnapshot Snapshot (Compiler a)
    | CompilerRequire (Identifier, Snapshot) (Compiler a)
    | CompilerError (CompilerErrors String)


--------------------------------------------------------------------------------
-- | A monad which lets you compile items and takes care of dependency tracking
-- for you.
newtype Compiler a = Compiler
    { unCompiler :: CompilerRead -> IO (CompilerResult a)
    }


--------------------------------------------------------------------------------
instance Functor Compiler where
    fmap f (Compiler c) = Compiler $ \r -> do
        res <- c r
        return $ case res of
            CompilerDone x w      -> CompilerDone (f x) w
            CompilerSnapshot s c' -> CompilerSnapshot s (fmap f c')
            CompilerRequire i c'  -> CompilerRequire i (fmap f c')
            CompilerError e       -> CompilerError e
    {-# INLINE fmap #-}


--------------------------------------------------------------------------------
instance Monad Compiler where
    return x = compilerResult $ CompilerDone x mempty
    {-# INLINE return #-}

    Compiler c >>= f = Compiler $ \r -> do
        res <- c r
        case res of
            CompilerDone x w    -> do
                res' <- unCompiler (f x) r
                return $ case res' of
                    CompilerDone y w'     -> CompilerDone y (w `mappend` w')
                    CompilerSnapshot s c' -> CompilerSnapshot s $ do
                        compilerTell w  -- Save dependencies!
                        c'
                    CompilerRequire i c'  -> CompilerRequire i $ do
                        compilerTell w  -- Save dependencies!
                        c'
                    CompilerError e       -> CompilerError e

            CompilerSnapshot s c' -> return $ CompilerSnapshot s (c' >>= f)
            CompilerRequire i c'  -> return $ CompilerRequire i (c' >>= f)
            CompilerError e       -> return $ CompilerError e
    {-# INLINE (>>=) #-}

    fail = compilerThrow . return
    {-# INLINE fail #-}


--------------------------------------------------------------------------------
instance Applicative Compiler where
    pure x = return x
    {-# INLINE pure #-}

    f <*> x = f >>= \f' -> fmap f' x
    {-# INLINE (<*>) #-}


--------------------------------------------------------------------------------
instance MonadUniverse Compiler where
    getMatches (Meta.Pattern p) = compilerGetMatches (Pattern p)

-- | Access provided metadata from anywhere
instance MonadMetadata Compiler where
    getMetadata = compilerGetMetadata


--------------------------------------------------------------------------------
-- | Compilation may fail with multiple error messages.
-- 'catchError' handles errors from 'throwError', 'fail' and 'Hexyll.Core.OldCompiler.noResult'
instance MonadError [String] Compiler where
    throwError = compilerThrow
    catchError c = compilerCatch c . (. compilerErrorMessages)


--------------------------------------------------------------------------------
-- | Like 'unCompiler' but treating IO exceptions as 'CompilerError's
runCompiler :: Compiler a -> CompilerRead -> IO (CompilerResult a)
runCompiler compiler read' = handle handler $ unCompiler compiler read'
  where
    handler :: SomeException -> IO (CompilerResult a)
    handler e = return $ CompilerError $ CompilationFailure $ show e :| []


--------------------------------------------------------------------------------
-- | Trying alternative compilers if the first fails, regardless whether through
-- 'fail', 'throwError' or 'Hexyll.Core.OldCompiler.noResult'.
-- Aggregates error messages if all fail.
instance Alternative Compiler where
    empty   = compilerNoResult []
    x <|> y = x `compilerCatch` (\rx -> y `compilerCatch` (\ry ->
        case (rx, ry) of
          (CompilationFailure xs,  CompilationFailure ys)  ->
            compilerThrow $ NonEmpty.toList xs ++ NonEmpty.toList ys
          (CompilationFailure xs,  CompilationNoResult ys) ->
            debug ys >> compilerThrow (NonEmpty.toList xs)
          (CompilationNoResult xs, CompilationFailure ys)  ->
            debug xs >> compilerThrow (NonEmpty.toList ys)
          (CompilationNoResult xs, CompilationNoResult ys) -> compilerNoResult $ xs ++ ys
        ))
      where
        debug = compilerDebugEntries "Hexyll.Core.OldCompiler.Internal: Alternative fail suppressed"
    {-# INLINE (<|>) #-}


--------------------------------------------------------------------------------
-- | Put the result back in a compiler
compilerResult :: CompilerResult a -> Compiler a
compilerResult x = Compiler $ \_ -> return x
{-# INLINE compilerResult #-}


--------------------------------------------------------------------------------
-- | Get the current environment
compilerAsk :: Compiler CompilerRead
compilerAsk = Compiler $ \r -> return $ CompilerDone r mempty
{-# INLINE compilerAsk #-}


--------------------------------------------------------------------------------
-- | Put a 'CompilerWrite'
compilerTell :: CompilerWrite -> Compiler ()
compilerTell = compilerResult . CompilerDone ()
{-# INLINE compilerTell #-}


--------------------------------------------------------------------------------
-- | Run an IO computation without dependencies in a Compiler
compilerUnsafeIO :: IO a -> Compiler a
compilerUnsafeIO io = Compiler $ \_ -> do
    x <- io
    return $ CompilerDone x mempty
{-# INLINE compilerUnsafeIO #-}


--------------------------------------------------------------------------------
-- | Throw errors in the 'Compiler'.
--
-- If no messages are given, this is considered a 'CompilationNoResult' error.
-- Otherwise, it is treated as a proper compilation failure.
compilerThrow :: [String] -> Compiler a
compilerThrow = compilerResult . CompilerError .
    maybe (CompilationNoResult []) CompilationFailure .
    NonEmpty.nonEmpty

-- | Put a 'CompilerError' with  multiple messages as 'CompilationNoResult'
compilerNoResult :: [String] -> Compiler a
compilerNoResult = compilerResult . CompilerError . CompilationNoResult


--------------------------------------------------------------------------------
-- | Allows to distinguish 'CompilerError's and branch on them with 'Either'
--
-- prop> compilerTry = (`compilerCatch` return . Left) . fmap Right
compilerTry :: Compiler a -> Compiler (Either (CompilerErrors String) a)
compilerTry (Compiler x) = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerDone res' w  -> return (CompilerDone (Right res') w)
        CompilerSnapshot s c -> return (CompilerSnapshot s (compilerTry c))
        CompilerRequire i c  -> return (CompilerRequire i (compilerTry c))
        CompilerError e      -> return (CompilerDone (Left e) mempty)
{-# INLINE compilerTry #-}


--------------------------------------------------------------------------------
-- | Allows you to recover from 'CompilerError's.
-- Uses the same parameter order as 'catchError' so that it can be used infix.
--
-- prop> c `compilerCatch` f = compilerTry c >>= either f return
compilerCatch :: Compiler a -> (CompilerErrors String -> Compiler a) -> Compiler a
compilerCatch (Compiler x) f = Compiler $ \r -> do
    res <- x r
    case res of
        CompilerDone res' w  -> return (CompilerDone res' w)
        CompilerSnapshot s c -> return (CompilerSnapshot s (compilerCatch c f))
        CompilerRequire i c  -> return (CompilerRequire i (compilerCatch c f))
        CompilerError e      -> unCompiler (f e) r
{-# INLINE compilerCatch #-}


--------------------------------------------------------------------------------
compilerDebugLog :: [String] -> Compiler ()
compilerDebugLog ms = do
  logger <- compilerLogger <$> compilerAsk
  compilerUnsafeIO $ forM_ ms $ Logger.debug logger

--------------------------------------------------------------------------------
-- | Pass a list of messages with a heading to the debug logger
compilerDebugEntries :: String -> [String] -> Compiler ()
compilerDebugEntries msg = compilerDebugLog . (msg:) . map indent
  where
    indent = unlines . map ("    "++) . lines


--------------------------------------------------------------------------------
compilerTellDependenciesCache :: [Dependency] -> [Identifier] -> Compiler ()
compilerTellDependenciesCache ds cs = do
  compilerDebugLog $ map (\d ->
      "Hexyll.Core.OldCompiler.Internal: Adding dependency: " ++ show d) ds
  compilerTell mempty {compilerDependencies = ds, compilerCache = cs}
{-# INLINE compilerTellDependenciesCache #-}


--------------------------------------------------------------------------------
compilerTellCacheHits :: Int -> Compiler ()
compilerTellCacheHits ch = compilerTell mempty {compilerCacheHits = ch}
{-# INLINE compilerTellCacheHits #-}


--------------------------------------------------------------------------------
compilerGetMetadata :: Identifier -> Compiler Metadata
compilerGetMetadata identifier = do
    provider <- compilerProvider <$> compilerAsk
    compilerTellDependenciesCache [Dependency $ fromIdentifier identifier] [identifier]
    compilerUnsafeIO $ resourceMetadata provider identifier

newtype Pattern = Pattern
  { unPattern :: PatternExpr
  } deriving ( Eq, Ord, Show, Typeable )

match :: Identifier -> Pattern -> Bool
match i (Pattern p) = matchExpr i p

instance IsString Pattern where
  fromString s = Pattern $ fromString s

--------------------------------------------------------------------------------
compilerGetMatches :: Pattern -> Compiler [Identifier]
compilerGetMatches pattern = do
    universe <- compilerUniverse <$> compilerAsk
    let matching = filter (`match` pattern) $ S.toList universe
        set'     = S.fromList matching
    compilerTellDependenciesCache [Dependency $ unPattern pattern] matching
    return matching
