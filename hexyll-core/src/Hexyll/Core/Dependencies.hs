--------------------------------------------------------------------------------
module Hexyll.Core.Dependencies where

import Prelude

import Data.Binary      ( Binary (..) )
import Data.Traversable ( for )
import Data.Typeable    ( Typeable )

import           Data.DList    ( DList, toList, singleton )
import           Data.List     ( find )
import qualified Data.Map as M
import           Data.Map      ( Map )
import           Data.Maybe    ( fromMaybe )
import qualified Data.Set as S
import           Data.Set      ( Set )

import Control.DeepSeq ( NFData (..) )

import Control.Monad                ( forM_, when )
import Control.Monad.Trans.RWS.Lazy ( RWS, rws, runRWS )

import Hexyll.Core.Identifier
import Hexyll.Core.Identifier.Pattern

-- | A dependency.
newtype Dependency = Dependency { unDependency :: PatternExpr }
  deriving (Eq, Ord, Show, Typeable)

-- | @since 0.1.0.0
instance Binary Dependency where
  put (Dependency x) = put x
  get = Dependency <$> get

-- | @since 0.1.0.0
instance NFData Dependency where
  rnf (Dependency x) = rnf x

-- | Dependency factors.
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

-- | Caches of dependency factors.
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
type IdentifierUniverse = [Identifier]

-- | A type of a list of outdated resources.
type IdentifierOutOfDate = Set Identifier

-- | A type of a log for 'outOfDate'.
type CalculationLog = [String]

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
data DependencyEnv = DependencyEnv
  { dependencyFacts    :: DependencyFacts
  , dependencyOldCache :: DependencyCache
  } deriving (Eq, Show, Typeable)

-- | A type of a state for 'outOfDate'.
data DependencyState = DependencyState
  { dependencyNewCache  :: DependencyCache
  , identifierOutOfDate :: IdentifierOutOfDate
  } deriving (Eq, Show, Typeable)

-- | A type of a log for 'outOfDate'.
type DependencyLog = DList String

type DependencyM = RWS DependencyEnv DependencyLog DependencyState

outOfDate' :: DependencyM ()
outOfDate' = do
  check
  bruteForce

getOutOfDate :: DependencyM IdentifierOutOfDate
getOutOfDate = rws $ \_ s -> case s of
  DependencyState dc io -> (io, DependencyState dc io, mempty)

markOutOfDate :: Identifier -> DependencyM ()
markOutOfDate i = rws $ \_ s -> case s of
  DependencyState dc io -> let io' = S.insert i io in
    io' `seq` ((), DependencyState dc io', mempty)

tellLog :: String -> DependencyM ()
tellLog l = rws $ \_ s -> ((), s, singleton l)

askFacts :: DependencyM DependencyFacts
askFacts = rws $ \r s -> case r of
  DependencyEnv df _ -> (df, s, mempty)

lookupFacts :: Identifier -> DependencyM [Dependency]
lookupFacts i = do
  facts <- askFacts
  return $ fromMaybe [] $ M.lookup i $ unDependencyFacts facts

askUniverse :: DependencyM IdentifierUniverse
askUniverse = M.keys . unDependencyFacts <$> askFacts

askOldCache :: DependencyM DependencyCache
askOldCache = rws $ \r s -> case r of
  DependencyEnv df dc -> (dc, s, mempty)

lookupOldCache :: Identifier -> DependencyM (Maybe [Identifier])
lookupOldCache i = do
  dc <- askOldCache
  return $ M.lookup i $ unDependencyCache dc

getNewCache :: DependencyM DependencyCache
getNewCache = rws $ \_ s -> case s of
  DependencyState dc io -> (dc, DependencyState dc io, mempty)

lookupNewCache :: Identifier -> DependencyM (Maybe [Identifier])
lookupNewCache i = do
  dc <- getNewCache
  return $ M.lookup i $ unDependencyCache dc

insertNewCache :: Identifier -> [Identifier] -> DependencyM ()
insertNewCache i is = rws $ \_ s -> case s of
  DependencyState dc io ->
    let
      dc' = DependencyCache $ M.insert i is $ unDependencyCache dc
    in
      dc' `seq` ((), DependencyState dc' io, mempty)

check :: DependencyM ()
check = do
  universe <- askUniverse
  oldCache <- askOldCache
  newCache <- getNewCache
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

dependenciesFor :: Identifier -> DependencyM [Identifier]
dependenciesFor i = do
  universe <- askUniverse
  ds <- lookupFacts i
  m_is <- lookupNewCache i
  case m_is of
    Nothing -> return $ concat $ for ds $ \d ->
      filter (`matchExpr` unDependency d) universe
    Just is -> return is

bruteForce :: DependencyM ()
bruteForce = do
  universe <- askUniverse
  bruteForce_0 universe

bruteForce_0 :: [Identifier] -> DependencyM ()
bruteForce_0 is = do
    (is', changed) <- bruteForce_1 is False
    when changed $ bruteForce_0 is'

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
