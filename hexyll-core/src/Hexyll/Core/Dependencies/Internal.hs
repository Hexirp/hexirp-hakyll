{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:      Hexyll.Core.Dependencies.Internal
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   internal
-- Portability: portable
--
-- This is an internal module for "Hexyll.Core.Dependencies".
module Hexyll.Core.Dependencies.Internal where

import Prelude

import Control.DeepSeq ( NFData (..) )
import Data.Binary     ( Binary (..) )
import Data.Typeable   ( Typeable )

import           Data.DList    ( DList, toList, singleton )
import           Data.List     ( find )
import qualified Data.Map as M
import           Data.Map      ( Map )
import           Data.Maybe    ( fromMaybe )
import qualified Data.Set as S
import           Data.Set      ( Set )

import Control.Monad    ( forM_, when )
import Data.Traversable ( for )

import Control.Monad.Trans.RWS.Lazy ( RWS, rws, runRWS )

import Hexyll.Core.Identifier
import Hexyll.Core.Identifier.Pattern

-- | A type of a dependency.
--
-- @since 0.1.0.0
newtype Dependency = Dependency { unDependency :: PatternExpr }
  deriving (Eq, Ord, Show, Typeable)

-- | @since 0.1.0.0
instance Binary Dependency where
  put (Dependency x) = put x
  get = Dependency <$> get

-- | @since 0.1.0.0
instance NFData Dependency where
  rnf (Dependency x) = rnf x

-- | A type of dependency factors.
--
-- @since 0.1.0.0
newtype DependencyFacts = DependencyFacts
  { unDependencyFacts :: Map Identifier [Dependency]
  } deriving (Eq, Show, Typeable)

-- | @since 0.1.0.0
instance Binary DependencyFacts where
  put (DependencyFacts x) = put x
  get = DependencyFacts <$> get

-- | @since 0.1.0.0
instance NFData DependencyFacts where
  rnf (DependencyFacts x) = rnf x

-- | A type of caches of dependency factors.
--
-- @since 0.1.0.0
newtype DependencyCache = DependencyCache
  { unDependencyCache :: Map Identifier [Identifier]
  } deriving (Eq, Show, Typeable)

-- | @since 0.1.0.0
instance Binary DependencyCache where
  put (DependencyCache x) = put x
  get = DependencyCache <$> get

-- | @since 0.1.0.0
instance NFData DependencyCache where
  rnf (DependencyCache x) = rnf x

-- | A type of a list of known resources.
--
-- @since 0.1.0.0
type IdentifierUniverse = [Identifier]

-- | A type of a list of outdated resources.
--
-- @since 0.1.0.0
type IdentifierOutOfDate = Set Identifier

-- | A type of a log for 'outOfDate'.
--
-- @since 0.1.0.0
type CalculationLog = [String]

-- | Calculate which resources need updating.
--
-- @since 0.1.0.0
outOfDate
  :: IdentifierOutOfDate
  -> DependencyFacts
  -> DependencyCache
  -> (IdentifierOutOfDate, DependencyCache, CalculationLog)
outOfDate io df dc =
  let
    env :: DependencyEnv
    env = DependencyEnv df dc
    initState :: DependencyState
    initState = DependencyState (DependencyCache M.empty) io
  in case runRWS outOfDate' env initState of
    ((), DependencyState dc' io', dl) -> (io', dc', toList dl)

-- | A type of an environment for 'outOfDate'.
--
-- @since 0.1.0.0
data DependencyEnv = DependencyEnv
  { dependencyFacts    :: DependencyFacts
  , dependencyOldCache :: DependencyCache
  } deriving (Eq, Show, Typeable)

-- | A type of a state for 'outOfDate'.
--
-- @since 0.1.0.0
data DependencyState = DependencyState
  { dependencyNewCache  :: DependencyCache
  , identifierOutOfDate :: IdentifierOutOfDate
  } deriving (Eq, Show, Typeable)

-- | A type of a log for 'outOfDate'.
--
-- @since 0.1.0.0
type DependencyLog = DList String

-- | A monad for 'outOfDate''.
--
-- @since 0.1.0.0
type DependencyM = RWS DependencyEnv DependencyLog DependencyState

-- | Calculate which resources need updating. It uses its own monad
-- 'DependencyM'.
--
-- This has two caches. 'dependencyOldCache', one of them is read-only.
--
-- @since 0.1.0.0
outOfDate' :: DependencyM ()
outOfDate' = do
  check
  bruteForce

-- | Get an 'IdentifierOutOfDate' on 'DependencyM'.
--
-- @since 0.1.0.0
getOutOfDate :: DependencyM IdentifierOutOfDate
getOutOfDate = rws $ \_ s -> case s of
  DependencyState dc io -> (io, DependencyState dc io, mempty)

-- | Mark an identifier as out-of-date on 'DependencyM'.
--
-- @since 0.1.0.0
markOutOfDate :: Identifier -> DependencyM ()
markOutOfDate i = rws $ \_ s -> case s of
  DependencyState dc io -> let io' = S.insert i io in
    io' `seq` ((), DependencyState dc io', mempty)

-- | Tell a string to the log on 'DependencyM'.
--
-- @since 0.1.0.0
tellLog :: String -> DependencyM ()
tellLog l = rws $ \_ s -> ((), s, singleton l)

-- | Ask the 'DependencyFacts' on 'DependencyM'.
--
-- @since 0.1.0.0
askFacts :: DependencyM DependencyFacts
askFacts = rws $ \r s -> case r of
  DependencyEnv df _ -> (df, s, mempty)

-- | Lookup the list of 'Dependency' at a key 'Identifier' in the
-- 'DependencyFacts' on 'DependencyM'.
--
-- @since 0.1.0.0
lookupFacts :: Identifier -> DependencyM [Dependency]
lookupFacts i = do
  facts <- askFacts
  return $ fromMaybe [] $ M.lookup i $ unDependencyFacts facts

-- | Ask the 'IdentifierUniverse' on 'DependencyM'.
--
-- @since 0.1.0.0
askUniverse :: DependencyM IdentifierUniverse
askUniverse = M.keys . unDependencyFacts <$> askFacts

-- | Ask the old 'DependencyCache' on 'DependencyM'.
--
-- @since 0.1.0.0
askOldCache :: DependencyM DependencyCache
askOldCache = rws $ \r s -> case r of
  DependencyEnv _ dc -> (dc, s, mempty)

-- | Lookup the list of 'Identifier' at a key 'Identifier' in the old
-- 'DependencyCache' on 'DependencyM'.
--
-- @since 0.1.0.0
lookupOldCache :: Identifier -> DependencyM (Maybe [Identifier])
lookupOldCache i = do
  dc <- askOldCache
  return $ M.lookup i $ unDependencyCache dc

-- | Get a new 'DependencyCache' on 'DependencyM'.
--
-- @since 0.1.0.0
getNewCache :: DependencyM DependencyCache
getNewCache = rws $ \_ s -> case s of
  DependencyState dc io -> (dc, DependencyState dc io, mempty)

-- | Lookup the list of 'Identifier' at a key 'Identifier' in a new
-- 'DependencyCache' on 'DependencyM'.
--
-- @since 0.1.0.0
lookupNewCache :: Identifier -> DependencyM (Maybe [Identifier])
lookupNewCache i = do
  dc <- getNewCache
  return $ M.lookup i $ unDependencyCache dc

-- | Insert a new key 'Identifier' and value @['Identifier']@ in a new
-- 'DependencyCache' on 'DependencyM'.
--
-- @since 0.1.0.0
insertNewCache :: Identifier -> [Identifier] -> DependencyM ()
insertNewCache i is = rws $ \_ s -> case s of
  DependencyState dc io ->
    let
      dc' = DependencyCache $ M.insert i is $ unDependencyCache dc
    in
      dc' `seq` ((), DependencyState dc' io, mempty)

-- | Check if it should be updated.
--
-- There are two conditions. When A meets any condition, it is marked.
--
-- * It is new.
-- * Resources it requires are changing.
--
-- @since 0.1.0.0
check :: DependencyM ()
check = do
  universe <- askUniverse
  forM_ universe $ \i -> do
    m_ois <- lookupOldCache i
    case m_ois of
      Nothing -> do
        tellLog $ show i ++ " is out-of-date because it is new"
        markOutOfDate i
      Just ois -> do
        nis <- dependenciesFor i
        insertNewCache i nis
        when (ois /= nis) $ do
          tellLog $ show i ++ " is out-of-date because its pattern changed"
          markOutOfDate i

-- | Calculate dependencies for an identifier.
--
-- This function uses 'dependencyNewCache' to memoize.
--
-- @since 0.1.0.0
dependenciesFor :: Identifier -> DependencyM [Identifier]
dependenciesFor i = do
  universe <- askUniverse
  ds <- lookupFacts i
  m_is <- lookupNewCache i
  case m_is of
    Nothing -> return $ concat $ for ds $ \d ->
      filter (`matchExpr` unDependency d) universe
    Just is -> return is

-- | Calculate out-of-dated resources by brute force.
--
-- This function has an rule:
--
-- * Resources that depend on the resource to be updated should be updated.
--
-- @since 0.1.0.0
bruteForce :: DependencyM ()
bruteForce = do
  universe <- askUniverse
  ood <- getOutOfDate
  bruteForce_0 $ filter (`S.notMember` ood) universe

-- | An internal function for 'bruteForce'.
--
-- @since 0.1.0.0
bruteForce_0 :: [Identifier] -> DependencyM ()
bruteForce_0 is = do
    (is', changed) <- bruteForce_1 is False
    when changed $ bruteForce_0 is'

-- | An internal function for 'bruteForce'.
--
-- @since 0.1.0.0
bruteForce_1 :: [Identifier] -> Bool -> DependencyM ([Identifier], Bool)
bruteForce_1 []        _ = return ([], False)
bruteForce_1 (iv : is) b = do
  (is', b') <- bruteForce_1 is b
  deps <- dependenciesFor iv
  ood <- getOutOfDate
  case find (`S.member` ood) deps of
    Nothing ->
      return (iv : is', b')
    Just idep -> do
      tellLog $ concat $
        [ show iv
        , " is out-of-date because "
        , show idep
        , " is out-of-date"
        ]
      markOutOfDate iv
      return (is', True)
