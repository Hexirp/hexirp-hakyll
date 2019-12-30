{-# LANGUAGE CPP #-}

module Hexyll.Core.IdentifierSpec (spec) where

  import Prelude

  import Test.Hspec
  import Test.QuickCheck

  import Control.Exception (evaluate)
  import Control.DeepSeq

  import Hexyll.Core.Identifier

  spec :: Spec
  spec = do

    describe "show @Identifier" $ do

      it "normally works" $ do
        show (fromFilePath "a.txt") `shouldBe` "a.txt"

      it "normally works with the version" $ do
        show (setIdentVersion (Just "pdf") $ fromFilePath "a.txt")
            `shouldBe` "a.txt (pdf)"

    describe "fromFilePath" $ do

      it "can parse 'foo.txt'" $ do
        rnf (fromFilePath "foo.txt") `shouldBe` ()

      it "can not parse 'foo/'" $ do
        (evaluate $ fromFilePath "foo/") `shouldThrow` anyErrorCall
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "can not parse 'C:\\foo.md'" $ do
        (evaluate $ fromFilePath "C:\\foo.md") `shouldThrow` anyErrorCall
#else

      it "can not parse '/foo.md'" $ do
        (evaluate $ fromFilePath "/foo.md") `shouldThrow` anyErrorCall
#endif

    describe "toFilePath" $ do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "returns a path with '\\' separated" $ do
        (toFilePath $ fromFilePath "foo\\bar.md") `shouldBe` "foo\\bar.md"

      it "returns a path with '\\' separated (posix style)" $ do
        (toFilePath $ fromFilePath "foo/bar.md") `shouldBe` "foo\\bar.md"
#else

      it "returns a path with '/' separated" $ do
        (toFilePath $ fromFilePath "foo/bar.md") `shouldBe` "foo/bar.md"
#endif

    describe "getIdentVersion" $ do

      it "follows the law 'get (set s i) === s'" $
        property prop_get_set_IdentVersion

    describe "setIdentVersion" $ do

      it "follows the law 'set (get i) i === i'" $
        property prop_set_get_IdentVersion

      it "follows the law 'set s0 (set s1 i) === set s0 i'" $
        property prop_set_set_IdentVersion


  -- Functions to test getIdentVersion and setIdentVersion

  -- The mock without a identifier version
  mock_Identifier_0 :: Identifier
  mock_Identifier_0 = fromFilePath "foo.c"

  -- The mock with a identifier version
  mock_Identifier_1 :: Maybe String -> Identifier
  mock_Identifier_1 s = setIdentVersion s mock_Identifier_0

  prop_get_set_IdentVersion :: Maybe String -> Bool
  prop_get_set_IdentVersion s =
    let
      i = mock_Identifier_0
    in
      getIdentVersion (setIdentVersion s i) == s

  prop_set_get_IdentVersion :: Maybe String -> Bool
  prop_set_get_IdentVersion s =
    let
      i = mock_Identifier_1 s
    in
      setIdentVersion (getIdentVersion i) i == i

  prop_set_set_IdentVersion :: Maybe String -> Maybe String -> Bool
  prop_set_set_IdentVersion s0 s1 =
    let
      i = mock_Identifier_0
    in
      setIdentVersion s0 (setIdentVersion s1 i) == setIdentVersion s0 i