{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.Writable
-- Copyright:   (c) 2020 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides a type class 'Writable'.
--
-- @since 0.1.0.0
module Hexyll.Core.Writable where

  import Prelude

  import System.IO (Handle, hPutStrLn)

  class Writable a where
    write :: Handle -> a -> IO ()

  instance Writable () where
    write _ _ = return ()

  instance Writable String where
    write h s = hPutStrLn h s
