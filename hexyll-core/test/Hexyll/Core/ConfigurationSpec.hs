{-# LANGUAGE TemplateHaskell #-}

module Hexyll.Core.ConfigurationSpec (spec) where

  import Prelude
  import Test.Hspec

  import Path

  import Hexyll.Core.Configuration

  spec :: Spec
  spec = do

    describe "defaultIgnoreFile" $ do

      it "ignore files is prefix of '.' ('.gitignore')" $ do
        defaultIgnoreFile $(mkRelFile ".gitignore") `shouldBe` True

      it "ignore files is prefix of '#' ('#Main.hs#')" $ do
        defaultIgnoreFile $(mkRelFile "#Main.hs#") `shouldBe` True

      it "ignore files is prefix of '#' ('#s')" $ do
        defaultIgnoreFile $(mkRelFile "#s") `shouldBe` True

      it "ignore files is prefix of '#' ('#')" $ do
        defaultIgnoreFile $(mkRelFile "#") `shouldBe` True

      it "ignore files is suffix of '~' ('~foo')" $ do
        defaultIgnoreFile $(mkRelFile "foo~") `shouldBe` True

      it "ignore files is suffix of '~' ('pya/~')" $ do
        defaultIgnoreFile $(mkRelFile "pya/~") `shouldBe` True

      it "ignore files is suffix of '~' ('mu/herobrine~')" $ do
        defaultIgnoreFile $(mkRelFile "mu/herobrine~") `shouldBe` True

      it "ignore files is suffix of '.swp'" $ do
        defaultIgnoreFile $(mkRelFile "a.txt.swp") `shouldBe` True

      it "do not ignore other files ('ma.')" $ do
        defaultIgnoreFile $(mkRelFile "ma.") `shouldBe` False

      it "do not ignore other files ('hash#')" $ do
        defaultIgnoreFile $(mkRelFile "hash#") `shouldBe` False

      it "do not ignore other files ('~foo')" $ do
        defaultIgnoreFile $(mkRelFile "~foo") `shouldBe` False

      it "do not ignore other files ('a.yxy.swp.log')" $ do
        defaultIgnoreFile $(mkRelFile "a.yxy.swp.log") `shouldBe` False
