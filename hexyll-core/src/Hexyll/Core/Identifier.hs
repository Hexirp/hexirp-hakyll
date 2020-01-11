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
module Hexyll.Core.Identifier
  ( Identifier
  , fromFilePath
  , toFilePath
  , getIdentVersion
  , setIdentVersion
  ) where

  import Prelude

  import Hexyll.Core.Identifier.Internal
