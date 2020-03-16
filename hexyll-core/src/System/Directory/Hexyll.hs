-- |
-- Module:      System.Directory.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes additional functions of "System.Directory".
module System.Directory.Hexyll
  ( inDir
  , listDirectoryRecursive
  ) where

  import Prelude

  import Path

  -- | @inDir path dir@ checks that @path@ is under @dir@. For example, @inDir
  -- "foo\/bar\/a.txt" "foo/"@ may be equal to @return True@.
  --
  -- @path@ should be a path to a file. @dir@ should be a path to a directory.
  -- 'parseRelDir' and `parseRelFile' check whether the condition is met.
  --
  -- >>> inDir "foo/a.txt" "foo/"
  -- True
  --
  -- >>> inDir "foo/bar/a.txt" "foo/"
  -- True
  --
  -- >>> inDir "foo/baz/a.txt" "foo/baa/"
  -- False
  --
  -- @since 0.1.0.0
  inDir :: FilePath -> FilePath -> IO Bool
  inDir path dir = do
    pa <- parseRelFile path
    di <- parseRelDir dir
    return $ di `isProperPrefixOf` pa

  listDirectoryRecursive :: Path Rel Dir -> IO [Path Rel File]
  listDirectoryRecursive p = do
    pb <- doesPathExist $ toFilePath p
    if pb
      then do
        p' <- listDirectory $ toFilePath p
        p'r <- forM p' $ \p'e -> do
          p'eb <- doesDirectoryExist p'v
          if p'eb
            then do
              p'ed <- parseRelDir p'e
              listDirectory $ p </> p'ed
            else do
              p'ef <- parseRelFile p'e
              return [p'ef]
        return $ concat p'r
      else
        error $ "listDirectoryRecursive: " ++ toFilePath p
