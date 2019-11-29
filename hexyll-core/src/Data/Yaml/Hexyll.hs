module Data.Yaml.Hexyll
    ( toString
    , toList
    ) where

import Prelude

import qualified Data.Text   as T
import qualified Data.Vector as V
import           Data.Yaml
import           Data.Scientific

-- | Convert 'Data.Yaml.Value' to 'String' for hexyll. If 'toString' apply to
-- a value which is not a scalar (is 'Object', 'Array', or 'Null'), will return
-- @Nothing@.
--
-- 'toString' make sure that numeric fields containing integer numbers are
-- shown as integers (i.e., "42" instead of "42.0").
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

toList :: Value -> Maybe [Value]
toList (Array a) = Just (V.toList a)
toList _         = Nothing
