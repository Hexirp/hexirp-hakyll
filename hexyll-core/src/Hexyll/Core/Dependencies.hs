--------------------------------------------------------------------------------
module Hexyll.Core.Dependencies
    ( Dependency (..)
    , DependencyFacts
    , outOfDate
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (foldM, forM_, unless, when)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS              (RWS, runRWS)
import qualified Control.Monad.State            as State
import           Control.Monad.Writer           (tell)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8)
import           Data.List                      (find)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Typeable                  (Typeable)

import Prelude
import Data.Traversable (for)

import Data.DList (DList, toList, singleton)

import Control.Monad.Trans.RWS.Lazy (RWS, rws, runRWS)

import Hexyll.Core.Identifier
import Hexyll.Core.Identifier.Pattern

-- | A dependency.
newtype Dependency = Dependency { unDependency :: PatternExpr }
  deriving (Eq, Show)

-- | Dependency factors.
newtype DependencyFacts = DependencyFacts
  { unDependencyFacts :: Map Identifier [Dependency] 
  } deriving (Eq, Show)

-- | Caches of dependency factors.
newtype DependencyCache = DependencyCache
  { unDependencyCache :: Map Identifier [Identifier]
  } deriving (Eq, Show)

-- | A type of a list of known resources.
type IdentifierUniverse = [Identifier]

-- | A type of a list of outdated resources.
type IdentifierOutOfDate = Set Identifier

-- | A type of a log for 'outOfDate'.
type CalculationLog = [String]

outOfDate
  :: IdentifierUniverse
  -> IdentifierOutOfDate
  -> DependencyFacts
  -> DependencyCache
  -> (IdentifierOutOfDate, DependencyCache, CalculationLog)
outOfDate iu io df dc =
  case runRWS outOfDate' (DependencyEnv df iu) (DependencyState dc io) of
    ((), DependencyState dc' io', log) -> (io', dc', toList log)

-- | A type of an environment for 'outOfDate'.
data DependencyEnv = DependencyEnv
  { dependencyFacts :: DependencyFacts
  , identifierUniverse :: IdentifierUniverse
  } deriving (Eq, Show)

-- | A type of a state for 'outOfDate'.
data DependencyState = DependencyState
  { dependencyCache     :: DependencyCache
  , identifierOutOfDate :: IdentifierOutOfDate
  } deriving (Eq, Show)

-- | A type of a log for 'outOfDate'.
type DependencyLog = DList String

type DependencyM = RWS DependencyEnv DependencyLog DependencyState

outOfDate' :: DependencyM ()
outOfDate' = do
  checkNew
  checkChangedPattern
  bruteForce

markOutOfDate :: Identifier -> DependencyM ()
markOutOfDate i = rws $ \_ s -> case s of
  DependencyState dc io -> let io' = S.insert i io in
    io' `seq` ((), DependencyState dc io', mempty)

tellLog :: String -> DependencyM ()
tellLog l = rws $ \_ s -> ((), s, singleton l)

askFacts :: DependencyM DependencyFacts
askFacts = rws $ \r s -> case r of
  DependencyEnv df _ -> (df, s, mempty)

askUniverse :: DependencyM IdentifierUniverse
askUniverse = M.keys . unDependencyFacts <$> askFacts

getCache :: DependencyM DependencyCache
getCache = rws $ \_ s -> case s of
  DependencyState dc io -> (dc, DependencyState dc io, mempty)

modifyCache :: (DependencyCache -> DependencyCache) -> DependencyM ()
modifyCache f = rws $ \_ s -> case s of
  DependencyState dc io -> let dc' = f dc in
    dc' `seq` ((), DependencyState dc' io, mempty)

checkNew :: DependencyM ()
checkNew = do
  universe <- askUniverse
  cache <- getCache
  forM_ universe $ \i ->
    unless (i `M.member` unDependencyCache cache) $ do
      tellLog $ show i ++ " is out-of-date because it is new"
      markOutOfDate i

dependenciesFor :: Identifier -> DependencyM [Identifier]
dependenciesFor i = do
  facts <- askFacts
  universe <- askUniverse
  return $ let ds = fromMaybe [] $ M.lookup i $ unDependencyFacts facts in
    concat $ for ds $ \d -> filter (`matchExpr` d) universe

dependenciesForCache :: Identifier -> DependencyM [Identifier]
dependenciesForCache i = do
  cache <- getCache
  return $ fromMaybe [] $ M.lookup i $ unDependencyCache cache

checkChangedPattern :: DependencyM ()
checkChangedPattern = do
  universe <- askUniverse
  forM_ universe $ \i -> do
    df <- dependenciesFor i
    dc <- dependenciesForCache i
    when (df /= dc) $ do
      tellLog $ show i ++ "is out-of-date because its pattern changed"
      markOutOfDate i
      modifyCache $ DependencyCache . M.insert i df . unDependencyCache

--------------------------------------------------------------------------------
bruteForce :: DependencyM ()
bruteForce = do
    todo <- ask
    go todo
  where
    go todo = do
        (todo', changed) <- foldM check ([], False) todo
        when changed (go todo')

    check (todo, changed) id' = do
        deps <- dependenciesFor id'
        ood  <- dependencyOod <$> State.get
        case find (`S.member` ood) deps of
            Nothing -> return (id' : todo, changed)
            Just d  -> do
                tell [show id' ++ " is out-of-date because " ++
                    show d ++ " is out-of-date"]
                markOod id'
                return (todo, True)
