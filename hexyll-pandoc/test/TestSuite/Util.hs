--------------------------------------------------------------------------------
-- | Test utilities
module TestSuite.Util
    ( fromAssertions
    , newTestStore
    , newTestProvider
    , testCompiler
    , testCompilerDone
    , testCompilerError
    , testConfiguration
    , cleanTestEnv
    , renderParagraphs
    ) where


--------------------------------------------------------------------------------
import           Data.List                     (intercalate, isInfixOf)
import           Data.Monoid                   (mempty)
import qualified Data.Set                      as S
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf                   (printf)


--------------------------------------------------------------------------------
import           Hexyll.Core.Compiler.Internal
import           Hexyll.Core.Configuration
import           Hexyll.Core.Identifier
import qualified Hexyll.Core.Logger            as Logger
import           Hexyll.Core.Provider
import           Hexyll.Core.Store             (Store)
import qualified Hexyll.Core.Store             as Store
import           Hexyll.Core.Util.File
import           Hexyll.Core.Item


--------------------------------------------------------------------------------
fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [TestTree]   -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]


--------------------------------------------------------------------------------
newTestStore :: IO Store
newTestStore = Store.new True $ storeDirectory testConfiguration


--------------------------------------------------------------------------------
newTestProvider :: Store -> IO Provider
newTestProvider store = newProvider store (const $ return False) $
    providerDirectory testConfiguration


--------------------------------------------------------------------------------
testCompiler :: Store -> Provider -> Identifier -> Compiler a
             -> IO (CompilerResult a)
testCompiler store provider underlying compiler = do
    logger <- Logger.new Logger.Error
    let read' = CompilerRead
            { compilerConfig     = testConfiguration
            , compilerUnderlying = underlying
            , compilerProvider   = provider
            , compilerUniverse   = S.empty
            , compilerRoutes     = mempty
            , compilerStore      = store
            , compilerLogger     = logger
            }

    result <- runCompiler compiler read'
    Logger.flush logger
    return result


--------------------------------------------------------------------------------
testCompilerDone :: Store -> Provider -> Identifier -> Compiler a -> IO a
testCompilerDone store provider underlying compiler = do
    result <- testCompiler store provider underlying compiler
    case result of
        CompilerDone x _    -> return x
        CompilerError e     -> fail $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " threw: " ++ intercalate "; " (compilerErrorMessages e)
        CompilerRequire i _ -> fail $
            "TestSuite.Util.testCompilerDone: compiler " ++ show underlying ++
            " requires: " ++ show i
        CompilerSnapshot _ _ -> fail
            "TestSuite.Util.testCompilerDone: unexpected CompilerSnapshot"

testCompilerError :: Store -> Provider -> Identifier -> Compiler a -> String -> IO ()
testCompilerError store provider underlying compiler expectedMessage = do
    result   <- testCompiler store provider underlying compiler
    case result of
        CompilerError e ->
            any (expectedMessage `isInfixOf`) (compilerErrorMessages e) @?
           "Expecting '" ++ expectedMessage ++ "' error"
        _               -> assertFailure "Expecting CompilerError"

--------------------------------------------------------------------------------
testConfiguration :: Configuration
testConfiguration = defaultConfiguration
    { destinationDirectory = "_testsite"
    , storeDirectory       = "_teststore"
    , tmpDirectory         = "_testtmp"
    , providerDirectory    = "test/data"
    }


--------------------------------------------------------------------------------
cleanTestEnv :: IO ()
cleanTestEnv = do
    removeDirectory $ destinationDirectory testConfiguration
    removeDirectory $ storeDirectory testConfiguration
    removeDirectory $ tmpDirectory testConfiguration


--------------------------------------------------------------------------------
-- | like 'Hexyll.Web.Pandoc.renderPandoc'
-- | but allowing to test without the @usePandoc@ flag
renderParagraphs :: Item String -> Compiler (Item String)
renderParagraphs = withItemBody (return
                       . intercalate "\n" -- no trailing line
                       . map (("<p>"++) . (++"</p>"))
                       . lines)
