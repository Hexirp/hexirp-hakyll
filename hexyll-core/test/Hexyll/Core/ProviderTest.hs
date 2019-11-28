--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Core.ProviderTest
    ( test_tests
    ) where


--------------------------------------------------------------------------------
import           Hexyll.Core.Metadata
import           Hexyll.Core.Provider
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (Assertion, testCase, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
test_tests :: TestTree
test_tests = testGroup "Hexyll.Core.ProviderTest"
    [ testCase "case01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = do
    store    <- newTestStore
    provider <- newTestProvider store
    True @=? resourceExists provider "example.md"

    metadata <- resourceMetadata provider "example.md"
    Just "An example"    @=? lookupString "title"    metadata
    Just "External data" @=? lookupString "external" metadata

    doesntExist <- resourceMetadata provider "doesntexist.md"
    mempty @=? doesntExist
    cleanTestEnv
