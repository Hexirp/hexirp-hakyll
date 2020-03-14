--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Web.Tags.Tests
    ( tests
    ) where

--------------------------------------------------------------------------------
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (Assertion, testCase, (@?=))

--------------------------------------------------------------------------------
import           Hexyll.Core.Identifier
import           Hexyll.Core.OldProvider
import           Hexyll.Core.OldStore           (Store)
import           Hexyll.Web.Tags
import           TestSuite.Util

tests :: TestTree
tests = testGroup "Hexyll.Web.Tags"
    [ testCase "testGetCategory" testGetCategory
    ]

testGetCategory :: Assertion
testGetCategory = do
    store    <- newTestStore
    provider <- newTestProvider store

    noCategory <- testCategoryDone store provider "example.md"
    noCategory @?= [""]

    oneCategory1 <- testCategoryDone store provider "posts/2010-08-26-birthday.md"
    oneCategory1 @?= ["posts"]

    oneCategory2 <- testCategoryDone store provider "posts/2019/05/10/tomorrow.md"
    oneCategory2 @?= ["10"]

    cleanTestEnv

--------------------------------------------------------------------------------
testCategoryDone :: Store -> Provider -> Identifier -> IO [String]
testCategoryDone store provider identifier =
    testCompilerDone store provider identifier $ getCategory identifier
