--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hakyll.Core.Runtime.Tests
import qualified Hakyll.Web.CompressCss.Tests
import qualified Hakyll.Web.Html.RelativizeUrls.Tests
import qualified Hakyll.Web.Html.Tests
#ifdef USE_PANDOC
import qualified Hakyll.Web.Pandoc.FileType.Tests
#endif
import qualified Hakyll.Web.Template.Context.Tests
import qualified Hakyll.Web.Template.Tests
import qualified Hakyll.Web.Tags.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Core.Runtime.Tests.tests
    , Hakyll.Web.CompressCss.Tests.tests
    , Hakyll.Web.Html.RelativizeUrls.Tests.tests
    , Hakyll.Web.Html.Tests.tests
#ifdef USE_PANDOC
    , Hakyll.Web.Pandoc.FileType.Tests.tests
#endif
    , Hakyll.Web.Tags.Tests.tests
    , Hakyll.Web.Template.Context.Tests.tests
    , Hakyll.Web.Template.Tests.tests
    ]
