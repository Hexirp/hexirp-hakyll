module Hexyll.Core.ConfigurationSpec (spec) where

  import Prelude
  import Test.Hspec

  import Hexyll.Core.Configuration

  spec :: Spec
  spec = do

    describe "defaultConfiguration" $ do

      it "has a destination directory '_site' in the field" $ do

        destinationDirectory defaultConfiguration `shouldBe` "_site"
