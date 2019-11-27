--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module TestSuite
    ( test_main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           ( TestTree
                                                      , testGroup
                                                      )


--------------------------------------------------------------------------------
import qualified Hexyll.Core.DependenciesTest
import qualified Hexyll.Core.IdentifierTest
import qualified Hexyll.Core.Provider.MetadataTest
import qualified Hexyll.Core.ProviderTest
import qualified Hexyll.Core.RoutesTest
import qualified Hexyll.Core.RulesTest
import qualified Hexyll.Core.StoreTest
import qualified Hexyll.Core.UnixFilterTest
import qualified Hexyll.Core.Util.StringTest


--------------------------------------------------------------------------------
test_main :: TestTree
test_main = testGroup "Hexyll"
    [ Hexyll.Core.DependenciesTest.tests
    , Hexyll.Core.IdentifierTest.tests
    , Hexyll.Core.Provider.MetadataTest.tests
    , Hexyll.Core.ProviderTest.tests
    , Hexyll.Core.RoutesTest.tests
    , Hexyll.Core.RulesTest.tests
    , Hexyll.Core.StoreTest.tests
    , Hexyll.Core.UnixFilterTest.tests
    , Hexyll.Core.Util.StringTest.tests
    ]
