module Hexyll.Core.Identifier.PatternSpec (spec) where

  import Prelude

  import Test.Hspec

  import Hexyll.Core.Identifier.Pattern

  spec :: Spec
  spec = do

    describe "fromString @PatternExpr" $ do

      it "normally works" $ do
        True `shouldBe` True
