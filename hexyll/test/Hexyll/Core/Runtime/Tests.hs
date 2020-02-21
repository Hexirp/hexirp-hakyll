--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Core.Runtime.Tests
    ( tests
    ) where

import Prelude
import Path

--------------------------------------------------------------------------------
import qualified Data.ByteString     as B
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.HUnit    (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Hexyll hiding (toFilePath)
import qualified Hexyll.Core.Logger  as Logger
import           Hexyll.Core.Runtime
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hexyll.Core.Runtime.Tests" $
    fromAssertions "run" [case01, case02]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    logger <- Logger.new Logger.Error
    _      <- run testConfiguration logger $ do
        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ do
                getResourceBody
                    >>= saveSnapshot "raw"
                    >>= renderParagraphs

        match (Pattern $ fromList ["partial.html", "partial-helper.html"]) $
            compile templateCompiler
        create ["partial.html.out"] $ do
            route idRoute
            compile $ do
                example <- loadSnapshotBody "example.md" "raw"
                makeItem example
                    >>= loadAndApplyTemplate "partial.html" defaultContext

        create ["bodies.txt"] $ do
            route idRoute
            compile $ do
                items <- loadAllSnapshots "*.md" "raw"
                makeItem $ concat $ map itemBody (items :: [Item String])

    favicon <- B.readFile $
        (toFilePath . providerDirectory) testConfiguration ++ "images/favicon.ico"
    favicon' <- B.readFile $
        (toFilePath . destinationDirectory) testConfiguration ++ "images/favicon.ico"
    favicon @?= favicon'

    example <- readFile $
        (toFilePath . destinationDirectory) testConfiguration ++ "example.html"
    lines example @?=  ["<p>This is an example.</p>"]

    bodies <- readFile $ (toFilePath . destinationDirectory) testConfiguration ++ "bodies.txt"
    head (lines bodies) @?=  "This is an example."

    partial  <- readFile $ (toFilePath . providerDirectory)    testConfiguration ++ "partial.html.out"
    partial' <- readFile $ (toFilePath . destinationDirectory) testConfiguration ++ "partial.html.out"
    partial @?= partial'

    cleanTestEnv


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = do
    logger <- Logger.new Logger.Error
    _      <- run testConfiguration logger $ do
        match "images/favicon.ico" $ do
            route   $ gsubRoute "images/" (const "")
            compile $ makeItem ("Test" :: String)

        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

    favicon <- readFile $
        (toFilePath . destinationDirectory) testConfiguration ++ "favicon.ico"
    favicon @?= "Test"

    cleanTestEnv
