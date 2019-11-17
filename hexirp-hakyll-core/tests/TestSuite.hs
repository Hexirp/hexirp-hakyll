--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Dependencies.Tests
import qualified Hakyll.Core.Identifier.Tests
import qualified Hakyll.Core.Provider.Metadata.Tests
import qualified Hakyll.Core.Provider.Tests
import qualified Hakyll.Core.Routes.Tests
import qualified Hakyll.Core.Rules.Tests
import qualified Hakyll.Core.Store.Tests
import qualified Hakyll.Core.UnixFilter.Tests
import qualified Hakyll.Core.Util.String.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Core.Dependencies.Tests.tests
    , Hakyll.Core.Identifier.Tests.tests
    , Hakyll.Core.Provider.Metadata.Tests.tests
    , Hakyll.Core.Provider.Tests.tests
    , Hakyll.Core.Routes.Tests.tests
    , Hakyll.Core.Rules.Tests.tests
    , Hakyll.Core.Store.Tests.tests
    , Hakyll.Core.UnixFilter.Tests.tests
    , Hakyll.Core.Util.String.Tests.tests
    ]
