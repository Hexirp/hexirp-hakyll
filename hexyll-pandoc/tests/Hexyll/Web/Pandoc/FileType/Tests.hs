--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hexyll.Web.Pandoc.FileType.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           ((@=?))


--------------------------------------------------------------------------------
import           Hexyll.Web.Pandoc.FileType
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hexyll.Web.Pandoc.FileType.Tests" $
    fromAssertions "fileType"
        [ Markdown                 @=? fileType "index.md"
        , Rst                      @=? fileType "about/foo.rst"
        , LiterateHaskell Markdown @=? fileType "posts/bananas.lhs"
        , LiterateHaskell LaTeX    @=? fileType "posts/bananas.tex.lhs"
        ]
