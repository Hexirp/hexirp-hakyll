--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hexyll.Core.Dependencies.Tests
import qualified Hexyll.Core.Identifier.Tests
import qualified Hexyll.Core.Provider.Metadata.Tests
import qualified Hexyll.Core.Provider.Tests
import qualified Hexyll.Core.Routes.Tests
import qualified Hexyll.Core.Rules.Tests
import qualified Hexyll.Core.Store.Tests
import qualified Hexyll.Core.UnixFilter.Tests
import qualified Hexyll.Core.Util.String.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hexyll"
    [ Hexyll.Core.Dependencies.Tests.tests
    , Hexyll.Core.Identifier.Tests.tests
    , Hexyll.Core.Provider.Metadata.Tests.tests
    , Hexyll.Core.Provider.Tests.tests
    , Hexyll.Core.Routes.Tests.tests
    , Hexyll.Core.Rules.Tests.tests
    , Hexyll.Core.Store.Tests.tests
    , Hexyll.Core.UnixFilter.Tests.tests
    , Hexyll.Core.Util.String.Tests.tests
    ]