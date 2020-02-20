-- |
-- Module:      Data.Yaml.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes types and functions about "Data.Yaml".
module Data.Yaml.Hexyll
  ( toString
  , toList
  , BinaryValue (..)
  ) where

  import Prelude

  import Data.Typeable ( Typeable )
  import Data.Coerce   ( coerce )
  import Data.Binary   ( Binary (..), putWord8, getWord8 )

  import qualified Data.Text           as T
  import qualified Data.Vector         as V
  import qualified Data.HashMap.Strict as HM
  import           Data.Scientific
  import           Data.Yaml

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

  -- | A wrapper for the instance @'Binary' 'Value'@.
  --
  -- @since 0.1.0.0
  newtype BinaryValue = BinaryValue
    { unBinaryValue :: Value
    } deriving ( Eq, Show, Typeable )

  -- | @since 0.1.0.0
  instance Binary BinaryValue where
    put (BinaryValue v) = case v of
      Object o -> do
        putWord8 0
        put $
          let
            coerce' :: [(T.Text, Value)] -> [(T.Text, BinaryValue)]
            coerce' = coerce
          in
            coerce' $ HM.toList o
      Array a -> do
        putWord8 1
        put $
          let
            coerce' :: [Value] -> [BinaryValue]
            coerce' = coerce
          in
            coerce' $ V.toList a
      String s -> do
        putWord8 2
        put s
      Number n -> do
        putWord8 3
        put n
      Bool b -> do
        putWord8 4
        put b
      Null ->
        putWord8 5
    get = do
      t <- getWord8
      case t of
        0 -> do
          o' <- get
          return $
            let
              coerce' :: [(T.Text, BinaryValue)] -> [(T.Text, Value)]
              coerce' = coerce
            in
              BinaryValue $ Object $ HM.fromList $ coerce' o'
        1 -> do
          a' <- get
          return $
            let
              coerce' :: [BinaryValue] -> [Value]
              coerce' = coerce
            in
              BinaryValue $ Array $ V.fromList $ coerce' a'
        2 -> do
          s <- get
          return $ BinaryValue $ String s
        3 -> do
          n <- get
          return $ BinaryValue $ Number n
        4 -> do
          b <- get
          return $ BinaryValue $ Bool b
        5 ->
          return $ BinaryValue Null
        _ ->
          error "Data.Binary.get: Invalid BinaryValue"
