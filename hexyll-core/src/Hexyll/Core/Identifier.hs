-- |
-- Module:      Hexyll.Core.Identifier
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Identifier', a type used to uniquely identify an item.
-- An identifier is conceptually similar to a file path. Examples of
-- identifiers are:
--
-- * @posts/foo.markdown@
-- * @index@
-- * @error/404@
--
-- A 'Identifier' value can have its version. The information about version is
-- used inside the library.
--
-- @since 0.1.0.0
module Hexyll.Core.Identifier
  ( Identifier
  , fromFilePath
  , ufromFilePath
  , toFilePath
  , getIdentifierVersion
  , setIdentifierVersion
  ) where

  import Prelude

  import Hexyll.Core.Identifier.Internal

  -- | An unsafe 'fromFilePath', parse an identifier from a string. The string
  -- should be a relative path to file.
  --
  -- 'ufromFilePath' is a partical function. You should be careful. I
  -- recommended to only use this function for constants.
  --
  -- > indexIdent = ufromFilePath "index.md"
  --
  -- @since 0.1.0.0
  ufromFilePath :: FilePath -> Identifier
  ufromFilePath s = case fromFilePath s of
    Left e -> error $ unwords
      [ "Identifier.ufromFilePath:"
      , "It's not a relative path to file."
      , "'fromFilePath' threw an error:"
      , show (show e)
      ]
    Right i -> i
