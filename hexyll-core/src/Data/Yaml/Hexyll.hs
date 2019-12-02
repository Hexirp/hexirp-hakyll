-- |
-- Module:      Data.Yaml.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes functions about "Data.Yaml".
module Data.Yaml.Hexyll
  ( toString
  , toList
  ) where

  import Prelude

  import qualified Data.Text   as T
  import qualified Data.Vector as V
  import           Data.Yaml
  import           Data.Scientific

  -- | Convert 'Value' to 'String' for hexyll. If 'toString' apply to a value
  -- which is not a scalar (is 'Object', 'Array', or 'Null'), will return
  -- @Nothing@.
  --
  -- 'toString' make sure that numeric fields containing integer numbers are
  -- shown as integers (i.e., "42" instead of "42.0").
  --
  -- >>> toString (String (T.pack "foo"))
  -- Just "foo"
  --
  -- >>> toString (Bool True)
  -- Just "true"
  --
  -- >>> toString (Number (scientific 12 1))
  -- Just "120"
  --
  -- >>> toString (Number (scientific 12 (-1)))
  -- Just "1.2"
  --
  -- >>> toString Null
  -- Nothing
  --
  -- @since 0.1.0.0
  toString :: Value -> Maybe String
  toString (String t)   = Just (T.unpack t)
  toString (Bool True)  = Just "true"
  toString (Bool False) = Just "false"
  toString (Number d)
    | isInteger d       = Just (formatScientific Fixed (Just 0) d)
    | otherwise         = Just (formatScientific Generic Nothing d)
  toString _            = Nothing

  -- | Convert 'Value' to @['Value']@ list for hexyll. If 'toString' apply to a
  -- value which is not 'Array', will return @Nothing@.
  --
  -- >>> toList (Array (V.fromList [Bool True, Bool False]))
  -- Just [Bool True,Bool False]
  --
  -- >>> toList (String (T.pack "foo"))
  -- Nothing
  --
  -- @since 0.1.0.0
  toList :: Value -> Maybe [Value]
  toList (Array a) = Just (V.toList a)
  toList _         = Nothing
