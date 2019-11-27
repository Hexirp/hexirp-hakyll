--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Core.RoutesTest
    ( test_tests
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe             (fromMaybe)
import           Hexyll.Core.Identifier
import           Hexyll.Core.Metadata
import           Hexyll.Core.Routes
import           System.FilePath        ((</>))
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
test_tests :: TestTree
test_tests = testGroup "Hexyll.Core.Routes.Tests" $ fromAssertions "runRoutes"
    [ testRoutes "foo.html" (setExtension "html") "foo"
    , testRoutes "foo.html" (setExtension ".html") "foo"
    , testRoutes "foo.html" (setExtension "html") "foo.markdown"
    , testRoutes "foo.html" (setExtension ".html") "foo.markdown"

    , testRoutes "neve ro ddo reven"
        (customRoute (reverse . toFilePath  )) "never odd or even"

    , testRoutes "foo" (constRoute "foo") "bar"

    , testRoutes "tags/bar.xml" (gsubRoute "rss/" (const "")) "tags/rss/bar.xml"
    , testRoutes "tags/bar.xml"
        (gsubRoute "rss/" (const "") `composeRoutes` setExtension "xml")
        "tags/rss/bar"

    , testRoutes "food/example.md" (metadataRoute $ \md -> customRoute $ \id' ->
        fromMaybe "?" (lookupString "subblog" md) </> toFilePath id')
        "example.md"
    ]


--------------------------------------------------------------------------------
testRoutes :: FilePath -> Routes -> Identifier -> Assertion
testRoutes expected r id' = do
    store      <- newTestStore
    provider   <- newTestProvider store
    (route, _) <- runRoutes r provider id'
    Just expected @=? route
    cleanTestEnv
