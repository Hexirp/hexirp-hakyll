module System.Directory.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import System.FilePath
  import System.Directory

  import System.Directory.Hexyll

  spec :: Spec
  spec = do

    describe "inDir" $ do

      it "works in normally files \"foo/a.txt\" and \"foo/\"" $ do
        inDir "foo/a.txt" "foo/" `shouldReturn` True

      it "works in normally files \"foo/bar/a.txt\" and \"foo/\"" $ do
        inDir "foo/bar/a.txt" "foo/" `shouldReturn` True

      it "works in normally files \"foo/baz/a.txt\" and \"foo/bar/\"" $ do
        inDir "foo/baz/a.txt" "foo/bar/" `shouldReturn` False

      it "works in normally files \"foo/baz/a.txt\" and \"foo\"" $ do
        inDir "foo/baz/a.txt" "foo" `shouldReturn` True

      it "works in normally files \"foo/bar\" and \"foo\"" $ do
        inDir "foo/bar" "foo" `shouldReturn` True

      it "works with difference path separators (windows v.s. unix)" $ do
        inDir "foo\\a.txt" "foo/" `shouldReturn` True

      it "works with difference path separators (unix v.s. windows)" $ do
        inDir "foo/bar/a.txt" "foo\\bar\\" `shouldReturn` True

      it "works with difference path separators (windows v.s. windows)" $ do
        inDir "foo\\a.txt" "foo\\" `shouldReturn` True

      it "works with the two special directories @.@ and @..@ (@.@)" $ do
        inDir "foo/./bar/a.txt" "foo/bar/" `shouldReturn` True

      it "works with the two special directories @.@ and @..@ (@..@)" $ do
        inDir "foo/bar/../baz/a.txt" "foo/baz" `shouldReturn` True

      it "works on absolute paths (absolute v.s. relative)" $ do
        flip shouldReturn True $ do
          path <- canonicalizePath "foo/a.txt"
          dir  <- return "foo/"
          inDir path dir

      it "works on absolute paths (relative v.s. absolute)" $ do
        flip shouldReturn True $ do
          path <- return "foo/a.txt"
          dir  <- canonicalizePath "foo/"
          inDir path dir

      it "works on absolute paths (absolute v.s. absolute)" $ do
        flip shouldReturn True $ do
          path <- canonicalizePath "foo/a.txt"
          dir  <- canonicalizePath "foo/"
          inDir path dir

      it "correctly works on prefix ('foobar/a.txt' and 'foo')" $ do
        inDir "foobar/a.txt" "foo" `shouldReturn` False
