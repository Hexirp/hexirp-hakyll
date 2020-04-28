{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module:      Hexyll.Core.Writable
-- Copyright:   (c) 2020 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: non-portable (ghc-extensions: FlexibleInstances)
--
-- This module provides a type class 'Writable'.
--
-- @since 0.1.0.0
module Hexyll.Core.Writable where

  import Prelude

  import Data.Word (Word8)

  import System.IO (Handle, hPutStrLn)

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.ByteString.Builder as BB

  -- | A type class for writable and printable types.
  --
  -- @since 0.1.0.0
  class Writable a where
    write :: Handle -> a -> IO ()

  -- | @since 0.1.0.0
  instance Writable () where
    write _ _ = return ()

  -- | @since 0.1.0.0
  instance Writable String where
    write h s = hPutStrLn h s

  -- | @since 0.1.0.0
  instance Writable BS.ByteString where
    write h s = BS.hPut h s

  -- | @since 0.1.0.0
  instance Writable BL.ByteString where
    write h s = BL.hPut h s

  -- | @since 0.1.0.0
  instance Writable BB.Builder where
    write h s = BB.hPutBuilder h s

  -- | @since 0.1.0.0
  instance Writable [Word8] where
    write h s = write h (foldMap BB.word8 s)
