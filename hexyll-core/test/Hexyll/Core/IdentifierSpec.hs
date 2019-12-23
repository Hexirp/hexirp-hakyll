{-# LANGUAGE CPP #-}

module Hexyll.Core.IdentifierSpec (spec) where

  import Prelude
  import Test.Hspec

  import Control.DeepSeq

  import Hexyll.Core.Identifier

  spec :: Spec
  spec = do

    describe "fromFilePath" $ do

      it "can parse 'foo.txt'" $ do
        rnf (fromFilePath "foo.txt") `shouldBe` ()

      it "can not parse 'foo/'" $ do
        (evaluate $ fromFilePath "foo/") `shouldThrow` anyErrorCall

      it "can not parse '/foo.md'" $ do
        (evaluate $ fromFilePath "/foo.md") `shouldThrow` anyErrorCall

    describe "toFilePath" $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "returns a path with '/' separated" $ do
        (toFilePath $ fromFilePath "foo\\bar.md") `shouldBe` "foo\\bar.md"

      it "returns a path with '/' separated (form posix)" $ do
        (toFilePath $ fromFilePath "foo/bar.md") `shouldBe` "foo\\bar.md"
#else

      it "returns a path with '/' separated" $ do
        (toFilePath $ fromFilePath "foo/bar.md") `shouldBe` "foo/bar.md"
#endif
