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

      it "works on normally files" $ do
        inDir "foo/a.txt" "foo/" `shouldReturn` True
