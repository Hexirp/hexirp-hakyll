{-# LANGUAGE TypeApplications #-}

module Data.List.HexyllSpec (spec) where

  import Prelude

  import Test.Hspec

  import Data.List.Hexyll

  spec :: Spec
  spec = do

    describe "breakWhen" $ do

      it "can see the tail" $ do
        breakWhen @Int (\xs -> length xs == 3) [1,2,3,4,1,2,3,4]
            `shouldBe` ([1,2,3,4,1], [2,3,4])

      it "can be used like 'break'" $ do
        breakWhen @Int (\xs -> head xs > 3) [1,2,3,4,1,2,3,4]
            `shouldBe` ([1,2,3], [4,1,2,3,4])

      it "applied to an empty list, then returns empty lists" $ do
        breakWhen @Int (\xs -> head xs > 3) []
            `shouldBe` ([], [])

      it "may return empty list on first element" $ do
        breakWhen @Int (const True) [1,2,3,4]
            `shouldBe` ([], [1,2,3,4])

      it "may return empty list on second element" $ do
        breakWhen @Int (const False) [1,2,3,4]
            `shouldBe` ([1,2,3,4], [])

      it "does not apply the predicate to an empty list" $ do
        breakWhen @Int (\xs -> if null xs then undefined else False) [1,2,3,4]
            `shouldBe` ([1,2,3,4], [])
