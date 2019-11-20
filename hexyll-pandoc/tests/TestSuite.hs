--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hexyll.Web.Pandoc.FileType.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hexyll"
    [ Hexyll.Web.Pandoc.FileType.Tests.tests
    ]
