--------------------------------------------------------------------------------
module Hexyll.Core.Runtime
    ( run
    ) where

import Prelude
import Path

--------------------------------------------------------------------------------
import           Control.Monad                 (unless, (>=>))
import           Control.Monad.Except          (ExceptT, runExceptT, throwError)
import           Control.Monad.Reader          (ask)
import           Control.Monad.RWS             (RWST, runRWST)
import           Control.Monad.State           (get, modify)
import           Control.Monad.Trans           (liftIO)
import           Data.List                     (intercalate)
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           System.Exit                   (ExitCode (..))


--------------------------------------------------------------------------------
import           Hexyll.Core.Compiler.Internal
import           Hexyll.Core.Compiler.Require
import           Hexyll.Core.Configuration
import           Hexyll.Core.Dependencies
import           Hexyll.Core.Identifier hiding (toFilePath)
import           Hexyll.Core.Item
import           Hexyll.Core.Item.SomeItem
import           Hexyll.Core.Logger            (Logger)
import qualified Hexyll.Core.Logger            as Logger
import           Hexyll.Core.Provider
import           Hexyll.Core.Routes
import           Hexyll.Core.Rules.Internal
import           Hexyll.Core.Store             (Store)
import qualified Hexyll.Core.Store             as Store
import           Hexyll.Core.Util.File
import           Hexyll.Core.Writable


--------------------------------------------------------------------------------
run :: Configuration -> Logger -> Rules a -> IO (ExitCode, RuleSet)
run config logger rules = do
    -- Initialization
    Logger.header logger "Initialising..."
    Logger.message logger "Creating store..."
    store <- Store.new (inMemoryCache config) $ toFilePath $ storeDirectory config
    Logger.message logger "Creating provider..."
    provider <- newProvider store (parseRelFile >=> shouldIgnoreFile config) $
        toFilePath $ providerDirectory config
    Logger.message logger "Running rules..."
    ruleSet  <- runRules rules provider

    -- Get old facts
    mOldCache <- Store.get store cacheKey
    let oldCache = case mOldCache of Store.Found c -> c
                                       _             -> DependencyCache $ M.empty

    -- Build runtime read/state
    let compilers = rulesCompilers ruleSet
        read'     = RuntimeRead
            { runtimeConfiguration = config
            , runtimeLogger        = logger
            , runtimeProvider      = provider
            , runtimeStore         = store
            , runtimeRoutes        = rulesRoutes ruleSet
            , runtimeUniverse      = M.fromList compilers
            }
        state     = RuntimeState
            { runtimeDone      = S.empty
            , runtimeSnapshots = S.empty
            , runtimeTodo      = M.empty
            , runtimeFacts     = DependencyFacts M.empty
            , runtimeCache     = oldCache
            }

    -- Run the program and fetch the resulting state
    result <- runExceptT $ runRWST build read' state
    case result of
        Left e          -> do
            Logger.error logger e
            Logger.flush logger
            return (ExitFailure 1, ruleSet)

        Right (_, s, _) -> do
            Store.set store cacheKey $ runtimeCache s

            Logger.debug logger "Removing tmp directory..."
            removeDirectory $ toFilePath $ tmpDirectory config

            Logger.flush logger
            return (ExitSuccess, ruleSet)
  where
    cacheKey = ["Hexyll.Core.Runtime.run", "cache"]


--------------------------------------------------------------------------------
data RuntimeRead = RuntimeRead
    { runtimeConfiguration :: Configuration
    , runtimeLogger        :: Logger
    , runtimeProvider      :: Provider
    , runtimeStore         :: Store
    , runtimeRoutes        :: Routes
    , runtimeUniverse      :: Map Identifier (Compiler SomeItem)
    }


--------------------------------------------------------------------------------
data RuntimeState = RuntimeState
    { runtimeDone      :: Set Identifier
    , runtimeSnapshots :: Set (Identifier, Snapshot)
    , runtimeTodo      :: Map Identifier (Compiler SomeItem)
    , runtimeFacts     :: DependencyFacts
    , runtimeCache     :: DependencyCache
    }


--------------------------------------------------------------------------------
type Runtime a = RWST RuntimeRead () RuntimeState (ExceptT String IO) a


--------------------------------------------------------------------------------
build :: Runtime ()
build = do
    logger <- runtimeLogger <$> ask
    Logger.header logger "Checking for out-of-date items"
    scheduleOutOfDate
    Logger.header logger "Compiling"
    pickAndChase
    Logger.header logger "Success"


--------------------------------------------------------------------------------
scheduleOutOfDate :: Runtime ()
scheduleOutOfDate = do
    logger   <- runtimeLogger   <$> ask
    provider <- runtimeProvider <$> ask
    universe <- runtimeUniverse <$> ask
    facts    <- runtimeFacts    <$> get
    cache    <- runtimeCache    <$> get
    todo     <- runtimeTodo     <$> get

    let identifiers = M.keys universe
        modified    = S.fromList $ flip filter identifiers $
            resourceModified provider

    let (ood, cache', msgs) = outOfDate modified facts cache
        todo'               = M.filterWithKey
            (\id' _ -> id' `S.member` ood) universe

    -- Print messages
    mapM_ (Logger.debug logger) msgs

    -- Update facts and todo items
    modify $ \s -> s
        { runtimeDone  = runtimeDone s `S.union`
            (S.fromList identifiers `S.difference` ood)
        , runtimeTodo  = todo `M.union` todo'
        , runtimeCache = cache'
        }


--------------------------------------------------------------------------------
pickAndChase :: Runtime ()
pickAndChase = do
    todo <- runtimeTodo <$> get
    case M.minViewWithKey todo of
        Nothing            -> return ()
        Just ((id', _), _) -> do
            chase [] id'
            pickAndChase


--------------------------------------------------------------------------------
chase :: [Identifier] -> Identifier -> Runtime ()
chase trail id'
    | id' `elem` trail = throwError $ "Hexyll.Core.Runtime.chase: " ++
        "Dependency cycle detected: " ++ intercalate " depends on "
            (map show $ dropWhile (/= id') (reverse trail) ++ [id'])
    | otherwise        = do
        logger   <- runtimeLogger        <$> ask
        todo     <- runtimeTodo          <$> get
        provider <- runtimeProvider      <$> ask
        universe <- runtimeUniverse      <$> ask
        routes   <- runtimeRoutes        <$> ask
        store    <- runtimeStore         <$> ask
        config   <- runtimeConfiguration <$> ask
        Logger.debug logger $ "Processing " ++ show id'

        let compiler = todo M.! id'
            read' = CompilerRead
                { compilerConfig     = config
                , compilerUnderlying = id'
                , compilerProvider   = provider
                , compilerUniverse   = M.keysSet universe
                , compilerRoutes     = routes
                , compilerStore      = store
                , compilerLogger     = logger
                }

        result <- liftIO $ runCompiler compiler read'
        case result of
            -- Rethrow error
            CompilerError e -> throwError $ case compilerErrorMessages e of
                [] -> "Compiler failed but no info given, try running with -v?"
                es -> intercalate "; " es

            -- Signal that a snapshot was saved ->
            CompilerSnapshot snapshot c -> do
                -- Update info. The next 'chase' will pick us again at some
                -- point so we can continue then.
                modify $ \s -> s
                    { runtimeSnapshots =
                        S.insert (id', snapshot) (runtimeSnapshots s)
                    , runtimeTodo      = M.insert id' c (runtimeTodo s)
                    }

            -- Huge success
            CompilerDone (SomeItem item) cwrite -> do
                -- Print some info
                let facts = compilerDependencies cwrite
                    cacheHits
                        | compilerCacheHits cwrite <= 0 = "updated"
                        | otherwise                     = "cached "
                Logger.message logger $ cacheHits ++ " " ++ show id'

                -- Sanity check
                unless (itemIdentifier item == id') $ throwError $
                    "The compiler yielded an Item with Identifier " ++
                    show (itemIdentifier item) ++ ", but we were expecting " ++
                    "an Item with Identifier " ++ show id' ++ " " ++
                    "(you probably want to call makeItem to solve this problem)"

                -- Write if necessary
                (mroute, _) <- liftIO $ runRoutes routes provider id'
                case mroute of
                    Nothing    -> return ()
                    Just route -> do
                        let path = (toFilePath . destinationDirectory) config ++ route
                        liftIO $ makeDirectories path
                        liftIO $ write path item
                        Logger.debug logger $ "Routed to " ++ path

                -- Save! (For load)
                liftIO $ save store item

                -- Update state
                modify $ \s -> s
                    { runtimeDone  = S.insert id' (runtimeDone s)
                    , runtimeTodo  = M.delete id' (runtimeTodo s)
                    , runtimeFacts = DependencyFacts $ M.insert id' facts (unDependencyFacts $ runtimeFacts s)
                    }

            -- Try something else first
            CompilerRequire dep c -> do
                -- Update the compiler so we don't execute it twice
                let (depId, depSnapshot) = dep
                done      <- runtimeDone <$> get
                snapshots <- runtimeSnapshots <$> get

                -- Done if we either completed the entire item (runtimeDone) or
                -- if we previously saved the snapshot (runtimeSnapshots).
                let depDone =
                        depId `S.member` done ||
                        (depId, depSnapshot) `S.member` snapshots

                modify $ \s -> s
                    { runtimeTodo = M.insert id'
                        (if depDone then c else compilerResult result)
                        (runtimeTodo s)
                    }

                -- If the required item is already compiled, continue, or, start
                -- chasing that
                Logger.debug logger $ "Require " ++ show depId ++
                    " (snapshot " ++ depSnapshot ++ "): " ++
                    (if depDone then "OK" else "chasing")
                if depDone then chase trail id' else chase (id' : trail) depId
