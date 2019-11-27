--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Core.RulesTest
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import qualified Data.Set                       as S
import           Hexyll.Core.Compiler
import           Hexyll.Core.File
import           Hexyll.Core.Identifier
import           Hexyll.Core.Identifier.Pattern
import           Hexyll.Core.Metadata
import           Hexyll.Core.Routes
import           Hexyll.Core.Rules
import           Hexyll.Core.Rules.Internal
import           System.FilePath                ((</>))
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (Assertion, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hexyll.Core.Rules.Tests" $ fromAssertions "runRules"
    [case01]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    ioref    <- newIORef False
    store    <- newTestStore
    provider <- newTestProvider store
    ruleSet  <- runRules (rules01 ioref) provider
    let identifiers     = S.fromList $ map fst $ rulesCompilers ruleSet
        routes          = rulesRoutes ruleSet
        checkRoute ex i =
            runRoutes routes provider i >>= \(r, _) -> Just ex @=? r

    -- Test that we have some identifiers and that the routes work out
    S.fromList expected @=? identifiers
    checkRoute "example.html"    "example.md"
    checkRoute "example.md"      (sv "raw" "example.md")
    checkRoute "example.md"      (sv "nav" "example.md")
    checkRoute "example.mv1"     (sv "mv1" "example.md")
    checkRoute "example.mv2"     (sv "mv2" "example.md")
    checkRoute "food/example.md" (sv "metadataMatch" "example.md")
    readIORef ioref >>= (True @=?)
    cleanTestEnv
  where
    sv g     = setVersion (Just g)
    expected =
        [                    "example.md"
        , sv "raw"           "example.md"
        , sv "metadataMatch" "example.md"
        , sv "nav"           "example.md"
        , sv "mv1"           "example.md"
        , sv "mv2"           "example.md"

        ,          "russian.md"
        , sv "raw" "russian.md"
        , sv "mv1" "russian.md"
        , sv "mv2" "russian.md"
        ]


--------------------------------------------------------------------------------
rules01 :: IORef Bool -> Rules ()
rules01 ioref = do
    -- Compile some posts
    match "*.md" $ do
        route $ setExtension "html"
        compile copyFileCompiler

    -- Yeah. I don't know how else to test this stuff?
    preprocess $ writeIORef ioref True

    -- Compile them, raw
    match "*.md" $ version "raw" $ do
        route idRoute
        compile getResourceString

    version "metadataMatch" $
        matchMetadata "*.md" (\md -> lookupString "subblog" md == Just "food") $ do
            route $ customRoute $ \id' -> "food" </> toFilePath id'
            compile getResourceString

    -- Regression test
    version "nav" $ match (fromList ["example.md"]) $ do
        route idRoute
        compile copyFileCompiler

    -- Another edge case: different versions in one match
    match "*.md" $ do
        version "mv1" $ do
            route $ setExtension "mv1"
            compile getResourceString
        version "mv2" $ do
            route $ setExtension "mv2"
            compile getResourceString