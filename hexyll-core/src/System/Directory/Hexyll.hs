{-# OPTIONS_HADDOCK show-extensions #-}

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
  ( listDirectoryRecursive
  ) where

  import Prelude

  import Control.Monad ( forM )

  import           Path
  import           System.Directory       ( listDirectory, doesDirectoryExist )
  import qualified System.FilePath as Raw ( (</>), dropTrailingPathSeparator )

  -- | Recursive 'listDirectory'.
  --
  -- @since 0.1.0.0
  listDirectoryRecursive :: Path Rel Dir -> IO [Path Rel File]
  listDirectoryRecursive dir =
    let path = toFilePath dir in do
      path_exist <- doesDirectoryExist path
      if path_exist
        then do
          result <- go $ Raw.dropTrailingPathSeparator path
          case forM result parseRelFile of
            Left e -> error $ unlines
              [ "listDirectoryRecursive: Something wrong happened."
              , "listDirectoryRecursive:   " ++ show (show e)
              ]
            Right files -> return files
        else
          error $ unlines
            [ "listDirectoryRecursive: This directory is not exist."
            , "listDirectoryRecursive:   " ++ path
            ]
    where
      go :: FilePath -> IO [FilePath]
      go x = do
        x' <- listDirectory x
        x'r <- forM x' $ \x'e -> let x'e_ = x Raw.</> x'e in do
          x'eb <- doesDirectoryExist x'e_
          if x'eb then go x'e_ else return [x'e_]
        return $ concat x'r
