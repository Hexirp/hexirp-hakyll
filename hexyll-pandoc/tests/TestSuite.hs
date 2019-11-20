--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
#ifdef USE_PANDOC
import qualified Hexyll.Web.Pandoc.FileType.Tests
#endif


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hexyll"
    [
#ifdef USE_PANDOC
      Hexyll.Web.Pandoc.FileType.Tests.tests
#endif
    ]
