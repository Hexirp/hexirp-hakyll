module Control.Monad.HexyllSpec (spec) where

  import Prelude
  import Control.Exception (evaluate)

  import Test.Hspec

  import Control.Monad.Hexyll

  spec :: Spec
  spec = do

    describe "orM" $ do

      it "is a version of 'or' lifted to a monad ([FFF]=F)" $ do
        orM [Just False, Just False, Just False] `shouldBe` Just False

      it "is a version of 'or' lifted to a monad ([FFT]=T)" $ do
        orM [Just False, Just False, Just True] `shouldBe` Just True

      it "is a version of 'or' lifted to a monad ([]=F)" $ do
        orM [] `shouldBe` Just False

      it "retains the short-circuiting behaveiour ([FTU]=T)" $ do
        orM [Just False, Just True, error "evaluated"] `shouldBe` Just True

      it "retains the short-circuiting behaveiour ([FFU]=U)" $ do
        evaluate (orM [Just False, Just False, error "evaluated"])
            `shouldThrow` errorCall "evaluated"
