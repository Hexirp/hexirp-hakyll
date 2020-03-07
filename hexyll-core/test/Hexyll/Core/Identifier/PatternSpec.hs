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
          matchExpr "index.md" (fromGlob "*") `shouldBe` True

        it "normally works ('foo/index.md' with '*')" $ do
          matchExpr "foo/index.md" (fromGlob "*") `shouldBe` False

        it "normally works ('foo/index.md' with '*/*.md')" $ do
          matchExpr "foo/index.md" (fromGlob "*/*.md") `shouldBe` True

        it "normally works ('foo/bar/alpha.md' with '**/alpha.md')" $ do
          matchExpr "foo/bar/alpha.md" (fromGlob "**/alpha.md") `shouldBe` True

        it "normally works ('alpha.md' with '**/alpha.md')" $ do
          matchExpr "alpha.md" (fromGlob "**/alpha.md") `shouldBe` True

        it "normally works ('a/b/c/foo.txt' with 'a/**/*.txt')" $ do
          matchExpr "a/b/c/foo.txt" (fromGlob "a/**/*.txt") `shouldBe` True

      describe "and fromRegex" $ do

        it "normally works ('index.md' with 'index.md')" $ do
          matchExpr "index.md" (fromRegex "index.md") `shouldBe` True

      describe "and fromVersion" $ do

        it "normally works ('index.md' with Nothing)" $ do
          matchExpr "index.md" (fromVersion Nothing) `shouldBe` True

      describe "and everything" $ do

        it "normally works" $ do
          property $ \s -> matchExpr (fromString s) everything == True

      describe "and (.&&.)" $ do

        it "normally works (everything .&&. everything)" $ do
          property $ let p = everything .&&. everything in \s ->
            matchExpr (fromString s) p == True
