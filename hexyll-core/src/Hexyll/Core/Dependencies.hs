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


--------------------------------------------------------------------------------
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern


-- | A dependency.
newtype Dependency = Dependency PatternExpr
-- | Dependency factors.
newtype DependencyFacts = DependencyFacts (Map Identifier [Dependency])
-- | Caches of dependency factors.
newtype DependencyCache = DependencyCache (Map Identifier [Identifier])

-- | A type of a list of known resources.
type IdentifierUniverse = [Identifier]
-- | A type of a list of outdated resources.
type IdentifierOutOfDate = Set Identifier
-- | A type of a log for 'outOfDate'.
type DependencyLog = DList String

outOfDate
  :: IdentifierUniverse
  -> IdentifierOutOfDate
  -> DependencyFacts
  -> DependencyCache
  -> (IdentifierOutOfDate, DependencyCache, DependencyLog)
outOfDate iu io df dc =
  case runRWS outOfDate' (DependencyEnv df iu) (DependencyState dc io) of
    ((), DependencyState dc' io', log) -> (io', dc', log)

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

type DependencyM = RWS DependencyEnv DependencyState DependencyLog

--------------------------------------------------------------------------------
markOod :: Identifier -> DependencyM ()
markOod id' = State.modify $ \s ->
    s {dependencyOod = S.insert id' $ dependencyOod s}


--------------------------------------------------------------------------------
dependenciesFor :: Identifier -> DependencyM [Identifier]
dependenciesFor id' = do
    facts <- dependencyFacts <$> State.get
    return $ concatMap dependenciesFor' $ fromMaybe [] $ M.lookup id' facts
  where
    dependenciesFor' (IdentifierDependency i) = [i]
    dependenciesFor' (PatternDependency _ is) = S.toList is


--------------------------------------------------------------------------------
checkNew :: DependencyM ()
checkNew = do
    universe <- ask
    facts    <- dependencyFacts <$> State.get
    forM_ universe $ \id' -> unless (id' `M.member` facts) $ do
        tell [show id' ++ " is out-of-date because it is new"]
        markOod id'


--------------------------------------------------------------------------------
checkChangedPatterns :: DependencyM ()
checkChangedPatterns = do
    facts <- M.toList . dependencyFacts <$> State.get
    forM_ facts $ \(id', deps) -> do
        deps' <- foldM (go id') [] deps
        State.modify $ \s -> s
            {dependencyFacts = M.insert id' deps' $ dependencyFacts s}
  where
    go _   ds (IdentifierDependency i) = return $ IdentifierDependency i : ds
    go id' ds (PatternDependency p ls) = do
        universe <- ask
        let ls' = S.fromList $ filter (`matchExpr` p) universe
        if ls == ls'
            then return $ PatternDependency p ls : ds
            else do
                tell [show id' ++ " is out-of-date because a pattern changed"]
                markOod id'
                return $ PatternDependency p ls' : ds


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
