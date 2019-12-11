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
  ) where

  import Prelude
  import Data.List        (isPrefixOf)
  import System.IO.Error  (catchIOError)

  import System.FilePath
  import System.Directory

  -- | @inDir path dir@ checks that @path@ is under @dir@. For example, @inDir
  -- "foo/bar/a.txt" "foo/"@ may be equal to @return True@.
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
  inDir path dir = isPrefixOf <$> canonicalize dir <*> canonicalize path
    where
      canonicalize :: FilePath -> IO FilePath
      canonicalize s =
        canonicalizePath s `catchIOError` \_ ->
          makeAbsolute s `catchIOError` \_ ->
            return $ normalise s
