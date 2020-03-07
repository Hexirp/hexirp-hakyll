{-# LANGUAGE OverloadedStrings #-}

module Hexyll.Core.Identifier.PatternSpec (spec) where

  import Prelude

  import Data.String ( IsString (..) )

  import Test.Hspec

  import Hexyll.Core.Identifier.Pattern

  spec :: Spec
  spec = do

    describe "fromString @PatternExpr" $ do

      it "normally works" $ do
        fromString "index.md" `shouldBe` fromGlob "index.md"

    describe "matchExpr" $ do

      describe "and fromGlob" $ do

        it "nornally works ('index.md' with 'index.md')" $ do
          matchExpr "index.md" (fromGlob "index.md") `shouldBe` True

        it "normally works ('index.md' with 'ind*.md')" $ do
          matchExpr "index.md" (fromGlob "ind*.md") `shouldBe` True

        it "normally works ('index.md' with 'assert.md')" $ do
          matchExpr "index.md" (fromGlob "assert.md") `shouldBe` False

        it "normally works ('index.md' with '*')" $ do
          matchExpr "index.md" (fromGlob "*") `shouldBe` False

        it "normally works ('foo/index.md' with '*/*.md')" $ do
          matchExpr "foo/index.md" (fromGlob "*/*.md") `shouldBe` True

        it "normally works ('foo/bar/alpha.md' with '**/alpha.md')" $ do
          matchExpr "foo/bar/alpha.md" (fromGlob "**/alpha.md") `shouldBe` True
