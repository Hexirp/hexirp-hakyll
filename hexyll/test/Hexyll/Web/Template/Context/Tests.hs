--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Web.Template.Context.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (Assertion, testCase, (@=?))


--------------------------------------------------------------------------------
import           Hexyll.Core.Compiler
import           Hexyll.Core.Identifier
import           Hexyll.Core.Provider
import           Hexyll.Core.OldStore           (Store)
import           Hexyll.Web.Template.Context
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hexyll.Web.Template.Context.Tests"
    [ testCase "testDateField" testDateField
    ]


--------------------------------------------------------------------------------
testDateField :: Assertion
testDateField = do
    store    <- newTestStore
    provider <- newTestProvider store

    date1 <- testContextDone store provider "example.md" "date" $
        dateField "date" "%B %e, %Y"
    date1 @=? "October 22, 2012"

    date2 <- testContextDone store provider
        "posts/2010-08-26-birthday.md" "date" $
            dateField "date" "%B %e, %Y"
    date2 @=? "August 26, 2010"

    date3 <- testContextDone store provider
        "posts/2018-09-26.md" "date" $
            dateField "date" "%B %e, %Y"
    date3 @=? "September 26, 2018"

    date4 <- testContextDone store provider
        "posts/2019/05/10/tomorrow.md" "date" $
            dateField "date" "%B %e, %Y"
    date4 @=? "May 10, 2019"
    cleanTestEnv


--------------------------------------------------------------------------------
testContextDone :: Store -> Provider -> Identifier -> String
                -> Context String -> IO String
testContextDone store provider identifier key context =
    testCompilerDone store provider identifier $ do
        item <- getResourceBody
        cf   <- unContext context key [] item
        case cf of
            StringField str -> return str
            _               -> error $
                "Hexyll.Web.Template.Context.Tests.testContextDone: " ++
                "expected StringField"
