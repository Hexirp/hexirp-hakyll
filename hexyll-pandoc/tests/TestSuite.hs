--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hexyll.Core.Runtime.Tests
import qualified Hexyll.Web.CompressCss.Tests
import qualified Hexyll.Web.Html.RelativizeUrls.Tests
import qualified Hexyll.Web.Html.Tests
#ifdef USE_PANDOC
import qualified Hexyll.Web.Pandoc.FileType.Tests
#endif
import qualified Hexyll.Web.Template.Context.Tests
import qualified Hexyll.Web.Template.Tests
import qualified Hexyll.Web.Tags.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hexyll"
    [ Hexyll.Core.Runtime.Tests.tests
    , Hexyll.Web.CompressCss.Tests.tests
    , Hexyll.Web.Html.RelativizeUrls.Tests.tests
    , Hexyll.Web.Html.Tests.tests
#ifdef USE_PANDOC
    , Hexyll.Web.Pandoc.FileType.Tests.tests
#endif
    , Hexyll.Web.Tags.Tests.tests
    , Hexyll.Web.Template.Context.Tests.tests
    , Hexyll.Web.Template.Tests.tests
    ]
