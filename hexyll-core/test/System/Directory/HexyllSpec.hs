{-# LANGUAGE CPP #-}

module System.Directory.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import Path

  import System.Directory.Hexyll

  spec :: Spec
  spec = do

    describe "listDirectoryRecursive" $ do

      it "works normally" $ do
        (parseRelDir "./test-data/System/Directory/Hexyll/listDirectoryRecursive/" >>= listDirectoryRecursive >>= return . map toFilePath) `shouldReturn` ["test-data\\System\\Directory\\Hexyll\\listDirectoryRecursive\\beta.txt","test-data\\System\\Directory\\Hexyll\\listDirectoryRecursive\\alpha\\alpha.txt"]
