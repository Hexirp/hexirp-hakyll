module System.Directory.HexyllSpec (spec) where

  import Prelude
  import Test.Hspec

  import System.FilePath
  import System.Directory
  import System.IO.Temp

  import System.Directory.Hexyll

  -- tmpDir/
  -- - foo/
  --   - bar/
  --     - a.txt
  --   - baz/
  --     - a.txt
  withTmpDir :: String -> (FilePath -> IO a) -> IO a
  withTmpDir = undefined

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
        inDir "foo/bar/../baz/a.txt" "foo/baz" `shouldReturn` False

      it "works on absolute paths (absolute v.s. relative)" $ do
        inDir <$> canonicalizePath "foo/a.txt" <*> return "foo/"
            `shouldReturn` True

      it "works on absolute paths (relative v.s. absolute)" $ do
        inDir <$> return "foo/a.txt" <*> canonicalizePath "foo/"
            `shouldReturn` True

      it "works on absolute paths (absolute v.s. absolute)" $ do
        inDir <$> canonicalizePath "foo/a.txt" <*> canonicalizePath "foo/"
            `shouldReturn` True
