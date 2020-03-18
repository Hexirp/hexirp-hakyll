{-# LANGUAGE CPP #-}

module System.Directory.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import Path

  import System.Directory.Hexyll

  spec :: Spec
  spec = do

    describe "listDirectoryRecursive" $ do

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "works normally" $ do
        (parseRelDir ".\\test-data\\System\\Directory\\Hexyll\\listDirectoryRecursive\\" >>= listDirectoryRecursive >>= return . map toFilePath) `shouldReturn` ["test-data\\System\\Directory\\Hexyll\\listDirectoryRecursive\\beta.txt","test-data\\System\\Directory\\Hexyll\\listDirectoryRecursive\\alpha\\alpha.txt"]
#else

      it "works normally" $ do
        (parseRelDir "./test-data/System/Directory/Hexyll/listDirectoryRecursive/" >>= listDirectoryRecursive >>= return . map toFilePath) `shouldReturn` ["test-data/System/Directory/Hexyll/listDirectoryRecursive/beta.txt","test-data/System/Directory/Hexyll/listDirectoryRecursive/alpha/alpha.txt"]
#endif
