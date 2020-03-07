{-# LANGUAGE OverloadedStrings #-}

module Hexyll.Core.Identifier.PatternSpec (spec) where

  import Prelude

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
