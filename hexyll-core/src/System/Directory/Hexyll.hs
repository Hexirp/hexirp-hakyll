module System.Directory.Hexyll
  ( inDir
  ) where

  import Prelude

  import System.FilePath
  import System.Directory

  inDir :: FilePath -> FilePath -> IO Bool
  inDir path dir = if isAbsolute path
    then do
      dir' <- canonicalizePath dir `catchIOError` \_ ->
        makeAbsolute dir `catchIOError` \_ ->
          return $ normalise dir
      return $ dir' `isPrefixOf` normalise path
    else
      return $ normalise dir `isPrefixOf` normalise path
