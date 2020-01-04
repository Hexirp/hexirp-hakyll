-- |
-- Module:      Hexyll.Core.Identifier.Pattern
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module defines a type for glob patterns.
module Hexyll.Core.Identifier.Pattern.Glob where

  import Prelude

  import Control.DeepSeq (NFData (..))
  import Data.String     (IsString (..))
  import Data.Binary     (Binary (..))

  import qualified System.FilePath.Glob as Glob

  -- | The wrapper of 'Glob.Pattern'.
  --
  -- @since 0.1.0.0
  data Pattern = Pattern { unPattern :: Glob.Pattern }
    deriving (Eq, Show)

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString = Pattern . fromString

  -- | @since 0.1.0.0
  instance Binary Pattern where
    put = put . Glob.decompile . unPattern
    get = Pattern . Glob.compile <$> get

  -- | @since 0.1.0.0
  instance NFData Pattern where
    rnf (Pattern x) = rnf (decompile x) `seq` ()
    -- This may be incomplete. But it is necessary there are no instances
    -- @'NFData' 'Glob.Pattern'@.

  -- | Match a 'String' with a 'Pattern'.
  --
  -- The function is a wrapper function of 'Glob.match'.
  --
  -- @since 0.1.0.0
  match :: String -> Pattern -> Bool
  match s (Pattern p) = Glob.match p s
