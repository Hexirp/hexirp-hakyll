module Control.Monad.Hexyll (spec) where

  import Prelude
  import Test.Hspec

  import Control.Monad.Hexyll

  spec :: Spec
  spec = do

    describe "orM" $ do

      it "is a version of 'or' lifted to a monad" $ do
        orM [Just False, Just False, Just False] `shouldBe` Just False
        orM [Just True, Just False] `shouldBe` Just True
        orM [Just False, Just False, Just True] `shouldBe` Just True
        orM [] `shouldBe` Just False

      it "retains the short-circuiting behaveiour" $ do
        orM [Just False, Just True, undefined] `shouldBe` Just True
        orM [Just False, Just False, undefined]
            `shouldThrow` errorCall "*** Exception: Prelude.undefined"
