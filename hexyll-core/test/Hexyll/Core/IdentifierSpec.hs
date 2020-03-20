{-# LANGUAGE CPP #-}

module Hexyll.Core.IdentifierSpec (spec) where

  import Prelude

  import Data.Maybe (isJust)

  import Test.Hspec
  import Test.QuickCheck

  import Control.Exception (evaluate)
  import Control.DeepSeq

  import Hexyll.Core.Identifier

  spec :: Spec
  spec = do

    describe "fromFilePath" $ do

      it "can parse 'foo.txt'" $ do
        isJust (fromFilePath "foo.txt") `shouldBe` True

      it "can not parse 'foo/'" $ do
        isJust (fromFilePath "foo/") `shouldBe` False
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "can not parse 'C:\\foo.md'" $ do
        isJust (fromFilePath "C:\\foo.md") `shouldBe` False
#else

      it "can not parse '/foo.md'" $ do
        isJust (fromFilePath "/foo.md") `shouldBe` False
#endif

    describe "ufromFilePath" $ do

      it "can parse 'foo.txt'" $ do
        rnf (ufromFilePath "foo.txt") `shouldBe` ()

      it "can not parse 'foo/'" $ do
        evaluate (ufromFilePath "foo/") `shouldThrow` anyErrorCall
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "can not parse 'C:\\foo.md'" $ do
        evaluate (ufromFilePath "C:\\foo.md") `shouldThrow` anyErrorCall
#else

      it "can not parse '/foo.md'" $ do
        evaluate (ufromFilePath "/foo.md") `shouldThrow` anyErrorCall
#endif

    describe "fromIdentifierToFilePath" $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "returns a path with '\\' separated" $ do
        fromIdentifierToFilePath (ufromFilePath "foo\\bar.md") `shouldBe` "foo\\bar.md"

      it "returns a path with '\\' separated (posix style)" $ do
        fromIdentifierToFilePath (ufromFilePath "foo/bar.md") `shouldBe` "foo\\bar.md"
#else

      it "returns a path with '/' separated" $ do
        fromIdentifierToFilePath (ufromFilePath "foo/bar.md") `shouldBe` "foo/bar.md"
#endif

    describe "getIdentifierVersion" $ do

      it "follows the law 'get (set s i) === s'" $
        property prop_get_set_IdentVersion

    describe "setIdentifierVersion" $ do

      it "follows the law 'set (get i) i === i'" $
        property prop_set_get_IdentVersion

      it "follows the law 'set s0 (set s1 i) === set s0 i'" $
        property prop_set_set_IdentVersion


  -- Functions to test getIdentVersion and setIdentVersion

  -- The mock without a identifier version
  mock_Identifier_0 :: Identifier
  mock_Identifier_0 = ufromFilePath "foo.c"

  -- The mock with a identifier version
  mock_Identifier_1 :: Maybe String -> Identifier
  mock_Identifier_1 s = setIdentifierVersion s mock_Identifier_0

  prop_get_set_IdentVersion :: Maybe String -> Bool
  prop_get_set_IdentVersion s =
    let
      i = mock_Identifier_0
    in
      getIdentifierVersion (setIdentifierVersion s i) == s

  prop_set_get_IdentVersion :: Maybe String -> Bool
  prop_set_get_IdentVersion s =
    let
      i = mock_Identifier_1 s
    in
      setIdentifierVersion (getIdentifierVersion i) i == i

  prop_set_set_IdentVersion :: Maybe String -> Maybe String -> Bool
  prop_set_set_IdentVersion s0 s1 =
    let
      i = mock_Identifier_0
    in
      setIdentifierVersion s0 (setIdentifierVersion s1 i) == setIdentifierVersion s0 i
