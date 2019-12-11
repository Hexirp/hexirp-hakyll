module Control.Monad.HexyllSpec (spec) where

  import Prelude
  import Control.Exception (evaluate)

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
        orM [Just False, Just True, error "evaluated"] `shouldBe` Just True
        evaluate (orM [Just False, Just False, error "evaluated"])
            `shouldThrow` errorCall "evaluated"
