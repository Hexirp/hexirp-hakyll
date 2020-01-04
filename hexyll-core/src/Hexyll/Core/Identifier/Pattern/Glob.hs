{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module:      Hexyll.Core.Identifier.Pattern
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: non-portable (compiler: GHC, extensions: DeriveGeneric)
--
-- This module defines a type for glob patterns.
module Hexyll.Core.Identifier.Pattern.Glob where

  import Prelude
  import GHC.Generics

  import Control.DeepSeq (NFData (..))
  import Data.String     (IsString (..))
  import Data.Binary     (Binary (..))

  import qualified System.FilePath.Glob as Glob

  data Pattern = Pattern { unPattern :: Glob.Pattern }
    deriving (Eq, Show, Generic)

  instance IsString Pattern where
    fromString = Pattern . fromString

  instance Binary Pattern where
    put = put . Glob.decompile . unPattern
    get = Pattern . Glob.compile <$> get

  instance NFData Pattern

  match :: String -> Pattern -> Bool
  match s (Pattern p) = Glob.match p s
