module Hexyll.Core.ConfigurationSpec (spec) where

  import Prelude
  import Test.Hspec

  import Hexyll.Core.Configuration

  spec :: Spec
  spec = do

    describe "defaultIgnoreFile" $ do

      it "ignore files is prefix of '.'" $ do
        defaultIgnoreFile "." `shouldBe` True
        defaultIgnoreFile ".." `shouldBe` True
        defaultIgnoreFile ".gitignore" `shouldBe` True

      it "ignore files is prefix of '#'" $ do
        defaultIgnoreFile "#Main.hs#" `shouldBe` True
        defaultIgnoreFile "#s" `shouldBe` True
        defaultIgnoreFile "#" `shouldBe` True

      it "ignore files is suffix of '~'" $ do
        defaultIgnoreFile "foo~" `shouldBe` True
        defaultIgnoreFile "pya/~" `shouldBe` True
        defaultIgnoreFile "mu/herobrine~" `shouldBe` True

      it "ignore files is suffix of '.swp'" $ do
        defaultIgnoreFile "a.txt.swp" `shouldBe` True

      it "do not ignore other files" $ do
        defaultIgnoreFile "ma." `shouldBe` False
        defaultIgnoreFile "hash#" `shouldBe` False
        defaultIgnoreFile "~foo" `shouldBe` False
        defaultIgnoreFile "a.yxy.swp.log" `shouldBe` False

      it "applied to a directory, returns False" $ do
        defaultIgnoreFile "eee/" `shouldBe` False
        defaultIgnoreFile "too/eee/" `shouldBe` False
        defaultIgnoreFile "foo~/" `shouldBe` False
