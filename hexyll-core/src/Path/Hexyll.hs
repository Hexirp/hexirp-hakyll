{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Path.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes types and functions about "Path".
--
-- @since 0.1.0.0
module Path.Hexyll where

  import Prelude

  import Control.Monad.Catch ( MonadThrow )

  import qualified Path
  import qualified System.FilePath as Raw ( replaceExtension )

  -- | If the file has an extension replace it with the given extension
  -- otherwise add the new extension to it.
  --
  -- @since 0.1.0.0
  replaceExtension
    :: MonadThrow m
    => String
    -> Path.Path Path.Rel Path.File
    -> m (Path.Path Path.Rel Path.File)
  replaceExtension ext path =
    Path.parseRelFile $ Raw.replaceExtension ext $ Path.toFilePath path
