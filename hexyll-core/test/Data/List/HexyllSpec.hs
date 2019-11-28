module Data.List.HexyllSpec (spec) where

  import Prelude

  import Test.Hspec

  import Data.List.Hexyll

  spec :: Spec
  spec = do
    describe "breakWhen" $ do
      it "can see the tail" $ do
        breakWhen (\xs -> length xs == 3) [1,2,3,4,1,2,3,4]
            `shouldBe` ([1,2,3,4,1], [2,3,4])
      it "can be used like 'break'" $ do
        breakWhen (\xs -> head xs > 3) [1,2,3,4,1,2,3,4]
            `shouldBe` ([1,2,3], [4,1,2,3,4])
      it "applied to an empty list, then returns empty lists" $ do
        breakWhen (\xs -> head xs > 3) [] `shouldBe` ([], [])
      it "may return empty list on first element" $ do
        breakWhen (const True) [1,2,3,4] `shouldBe` ([], [1,2,3,4])
      it "may return empty list on second element" $ do
        breakWhen (const False) [1,2,3,4] `shouldBe` ([1,2,3,4], [])
      it "does not apply @p@ to an empty list" $ do
        breakWhen (\xs -> if null xs then undefined else False)
            `shouldBe` ([1,2,3,4], [])
