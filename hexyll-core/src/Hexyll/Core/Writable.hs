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

  import System.IO (Handle, hPutChar, hPutStr)

  import qualified Data.ByteString as BS
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.ByteString.Builder as BB

  -- | A type class for a writable and printable type.
  --
  -- @since 0.1.0.0
  class Writable a where
    -- | Write a value.
    --
    -- It is implemented by 'hPutStr', 'BS.hPut', etc.
    --
    -- @since 0.1.0.0
    write :: Handle -> a -> IO ()

  -- | @since 0.1.0.0
  instance Writable Char where
    write h s = hPutChar h s

  -- | @since 0.1.0.0
  instance Writable Word8 where
    write h s = write h (BB.word8 s)

  -- | @since 0.1.0.0
  instance Writable () where
    write _ _ = return ()

  -- | @since 0.1.0.0
  instance (Writable a, Writable b) => Writable (a, b) where
    write h (s0, s1) = write h s0 >> write h s1

  -- | @since 0.1.0.0
  instance {-# OVERLAPPABLE #-} Writable a => Writable [a] where
    write h s = foldMap (write h) s

  -- | @since 0.1.0.0
  instance Writable String where
    write h s = hPutStr h s

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
