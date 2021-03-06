module Data.Yaml.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import qualified Data.Text as T
  import qualified Data.Vector as V
  import qualified Data.HashMap.Strict as H
  import           Data.Yaml
  import           Data.Scientific

  import Data.Yaml.Hexyll

  spec :: Spec
  spec = do

    describe "toString" $ do

      it "works at String" $ do
        toString (String (T.pack "foo")) `shouldBe` Just "foo"

      it "works at Bool (true)" $ do
        toString (Bool True)  `shouldBe` Just "true"

      it "works at Bool (false)" $ do
        toString (Bool False) `shouldBe` Just "false"

      it "works at Number (integer)" $ do
        toString (Number (scientific 12 1)) `shouldBe` Just "120"

      it "works at Number (fraction)" $ do
        toString (Number (scientific 12 (-1))) `shouldBe` Just "1.2"

      it "works at Number (too big integer)" $ do
        toString (Number (scientific 12 7)) `shouldBe` Just "120000000"

      it "works at Number (too small fraction)" $ do
        toString (Number (scientific 12 (-7))) `shouldBe` Just "1.2e-6"

      it "does not work at Array" $ do
        toString (Array V.empty) `shouldBe` Nothing

      it "does not work at Object" $ do
        toString (Object H.empty) `shouldBe` Nothing

      it "does not work at Null" $ do
        toString Null `shouldBe` Nothing

    describe "toList" $ do

      it "works at Array" $ do
        toList (Array (V.fromList [Bool True, Bool False]))
            `shouldBe` Just [Bool True, Bool False]

      it "does not work at something that is not Array (string)" $ do
        toList (String (T.pack ""))      `shouldBe` Nothing

      it "does not work at something that is not Array (bool)" $ do
        toList (Bool True)               `shouldBe` Nothing

      it "does not work at something that is not Array (number)" $ do
        toList (Number (scientific 1 0)) `shouldBe` Nothing

      it "does not work at something that is not Array (object)" $ do
        toList (Object H.empty)          `shouldBe` Nothing

      it "does not work at something that is not Array (null)" $ do
        toList Null                      `shouldBe` Nothing
