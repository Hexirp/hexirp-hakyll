module Hexyll.Core.ConfigurationSpec (spec) where

  import Prelude
  import Test.Hspec

  import Hexyll.Core.Configuration

  spec :: Spec
  spec = do

    describe "defaultIgnoreFile" $ do

      it "ignore files is prefix of '.' ('.')" $ do
        defaultIgnoreFile "." `shouldBe` True

      it "ignore files is prefix of '.' ('..')" $ do
        defaultIgnoreFile ".." `shouldBe` True

      it "ignore files is prefix of '.' ('.gitignore')" $ do
        defaultIgnoreFile ".gitignore" `shouldBe` True

      it "ignore files is prefix of '#' ('#Main.hs#')" $ do
        defaultIgnoreFile "#Main.hs#" `shouldBe` True

      it "ignore files is prefix of '#' ('#s')" $ do
        defaultIgnoreFile "#s" `shouldBe` True

      it "ignore files is prefix of '#' ('#')" $ do
        defaultIgnoreFile "#" `shouldBe` True

      it "ignore files is suffix of '~' ('~foo')" $ do
        defaultIgnoreFile "foo~" `shouldBe` True

      it "ignore files is suffix of '~' ('pya/~')" $ do
        defaultIgnoreFile "pya/~" `shouldBe` True

      it "ignore files is suffix of '~' ('mu/herobrine~')" $ do
        defaultIgnoreFile "mu/herobrine~" `shouldBe` True

      it "ignore files is suffix of '.swp'" $ do
        defaultIgnoreFile "a.txt.swp" `shouldBe` True

      it "do not ignore other files ('ma.')" $ do
        defaultIgnoreFile "ma." `shouldBe` False

      it "do not ignore other files ('hash#')" $ do
        defaultIgnoreFile "hash#" `shouldBe` False

      it "do not ignore other files ('~foo')" $ do
        defaultIgnoreFile "~foo" `shouldBe` False

      it "do not ignore other files ('a.yxy.swp.log')" $ do
        defaultIgnoreFile "a.yxy.swp.log" `shouldBe` False

      it "applied to a directory, returns False ('eee/')" $ do
        defaultIgnoreFile "eee/" `shouldBe` False

      it "applied to a directory, returns False ('too/eee/')" $ do
        defaultIgnoreFile "too/eee/" `shouldBe` False

      it "applied to a directory, returns False ('foo~/')" $ do
        defaultIgnoreFile "foo~/" `shouldBe` False
