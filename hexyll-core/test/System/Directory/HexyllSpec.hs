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

    describe "inDir"

      it "works in normally files" $ do
        inDir "foo/a.txt" "foo/" `shouldReturn` True
        inDir "foo/bar/a.txt" "foo/" `shouldReturn` True
        inDir "foo/baz/a.txt" "foo/baa/" `shouldReturn` False
        inDir "foo/baz/a.txt" "foo" `shouldReturn` False
        inDir "foo/bar" "foo" `shouldReturn` True

      it "works with difference path separators" $ do
        inDir "foo\\a.txt" "foo/" `shouldReturn` True
        inDir "foo/bar/a.txt" "foo\\bar\\" `shouldReturn` True
