{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:      Hexyll.Core.Identifier.Pattern
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   internal
-- Portability: portable
--
-- This module defines a type for glob patterns.
--
-- @since 0.1.0.0
module Hexyll.Core.Identifier.Pattern.Glob where

  import Prelude

  import Data.Function (on)

  import Control.DeepSeq (NFData (..))
  import Data.String     (IsString (..))
  import Data.Binary     (Binary (..))

  import qualified System.FilePath.Glob as Glob

  -- | The wrapper of 'Glob.Pattern'.
  --
  -- @since 0.1.0.0
  newtype Pattern = Pattern { unPattern :: Glob.Pattern }
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance Ord Pattern where
    compare (Pattern x) (Pattern y) = (compare `on` Glob.decompile) x y

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString = Pattern . fromString

  -- | @since 0.1.0.0
  instance Binary Pattern where
    put (Pattern x) = put $ Glob.decompile x
    get = Pattern . Glob.compile <$> get

  -- | @since 0.1.0.0
  instance NFData Pattern where
    rnf (Pattern x) = rnf (Glob.decompile x) `seq` ()
    -- This may be incomplete. But it is necessary there are no instances
    -- @'NFData' 'Glob.Pattern'@.

  -- | Compile a 'String' to a 'Pattern'.
  --
  -- The function is a wrapper function of 'Glob.compile'.
  --
  -- @since 0.1.0.0
  compile :: String -> Pattern
  compile = Pattern . Glob.compile

  -- | Decompile a 'Pattern' to a 'String'.
  --
  -- The function is a wrapper function of 'Glob.decompile'.
  --
  -- @since 0.1.0.0
  decompile :: Pattern -> String
  decompile = Glob.decompile . unPattern

  -- | Match a 'String' with a 'Pattern'.
  --
  -- The function is a wrapper function of 'Glob.match'.
  --
  -- @since 0.1.0.0
  match :: String -> Pattern -> Bool
  match s (Pattern p) = Glob.match p s
