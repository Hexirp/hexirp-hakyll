{-# LANGUAGE CPP #-}

module System.Directory.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import System.FilePath
  import System.Directory

  import System.Directory.Hexyll

  spec :: Spec
  spec = do

    describe "inDir" $ do

      it "works in normally files 'foo/a.txt' and 'foo/'" $ do
        inDir "foo/a.txt" "foo/" `shouldReturn` True

      it "works in normally files 'foo/bar/a.txt' and 'foo/'" $ do
        inDir "foo/bar/a.txt" "foo/" `shouldReturn` True

      it "works in normally files 'foo/baz/a.txt' and 'foo/bar/'" $ do
        inDir "foo/baz/a.txt" "foo/bar/" `shouldReturn` False

      it "works in normally files 'foo/baz/a.txt' and 'foo" $ do
        inDir "foo/baz/a.txt" "foo" `shouldReturn` True

      it "works in normally files 'foo/bar' and 'foo'" $ do
        inDir "foo/bar" "foo" `shouldReturn` True
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

      it "works with difference path separators (windows v.s. posix)" $ do
        inDir "foo\\a.txt" "foo/" `shouldReturn` True

      it "works with difference path separators (posix v.s. windows)" $ do
        inDir "foo/bar/a.txt" "foo\\bar\\" `shouldReturn` True

      it "works with difference path separators (windows v.s. windows)" $ do
        inDir "foo\\a.txt" "foo\\" `shouldReturn` True
#endif

      it "works with the special directory @.@" $ do
        inDir "foo/./bar/a.txt" "foo/bar/" `shouldReturn` True

      it "correctly works on prefix ('foobar/a.txt' and 'foo')" $ do
        inDir "foobar/a.txt" "foo" `shouldReturn` False
