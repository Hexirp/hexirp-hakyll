{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.Identifier
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Identifier', a type used to uniquely identify an item
-- (compilation result).
--
-- @since 0.1.0.0
module Hexyll.Core.Identifier where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Control.Monad.Catch ( MonadThrow )

  import Data.String ( IsString, fromString )

  import Data.Binary ( Binary (..) )

  import Path

  import Hexyll.Core.Resource

  -- | A type used to uniquely identify an item (compilation result).
  --
  -- 'identifierPath' is a 'Resource' to be compiled. 'identifierVersion' is
  -- a version of compilation result.
  --
  -- 'identifierVersion' is used when you want to apply two kinds of compilers
  -- to the same resource.
  --
  -- @since 0.1.0.0
  data Identifier = Identifier
    { identifierPath    :: !Resource
    , identifierVersion :: !(Maybe String)
    } deriving (Eq, Ord, Show, Typeable)

  -- | @since 0.1.0.0
  instance Binary Identifier where
    put (Identifier p v) = do
      put p
      put v
    get = do
      v <- get
      p <- get
      return (Identifier p v)

  -- | @since 0.1.0.0
  instance IsString Identifier where
    fromString s = case fromFilePath s of
      Left e -> error $ unwords
        [ "fromString @Identifier:"
        , "It's not a relative path to file."
        , "'fromFilePath' threw an error:"
        , show (show e)
        ]
      Right i -> i

  -- | @since 0.1.0.0
  instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()

  -- | Make an identifier from a 'Resource'.
  --
  -- @since 0.1.0.0
  fromResource :: Resource -> Identifier
  fromResource p = Identifier p Nothing

  -- | Make an identifier from a path.
  --
  -- @since 0.1.0.0
  fromPath :: Path Rel File -> Identifier
  fromPath = fromResource . Resource

  -- | Parse an identifier from a string. The string should be a relative path
  -- to file.
  --
  -- @since 0.1.0.0
  fromFilePath :: MonadThrow m => FilePath -> m Identifier
  fromFilePath s = fmap fromPath $ parseRelFile s

  -- | An unsafe 'fromFilePath', parse an identifier from a string. The string
  -- should be a relative path to file.
  --
  -- 'ufromFilePath' is a partical function. You should be careful. I
  -- recommended to only use this function for constants.
  --
  -- > indexIdent = ufromFilePath "index.md"
  --
  -- @since 0.1.0.0
  ufromFilePath :: FilePath -> Identifier
  ufromFilePath s = case fromFilePath s of
    Left e -> error $ unwords
      [ "Identifier.ufromFilePath:"
      , "It's not a relative path to file."
      , "'fromFilePath' threw an error:"
      , show (show e)
      ]
    Right i -> i

  -- | Convert an identifier to a 'Resource'.
  --
  -- @since 0.1.0.0
  fromIdentifierToResource :: Identifier -> Resource
  fromIdentifierToResource = identifierPath

  -- | Convert an identifier to a path.
  --
  -- @since 0.1.0.0
  fromIdentifierToPath :: Identifier -> Path Rel File
  fromIdentifierToPath = unResource . fromIdentifierToResource

  -- | Convert an identifier to a relative 'FilePath'.
  --
  -- @since 0.1.0.0
  fromIdentifierToFilePath :: Identifier -> FilePath
  fromIdentifierToFilePath = toFilePath . fromIdentifierToPath

  -- | Get the version of an identifier. I recommend that you do not use this
  -- function.
  --
  -- @since 0.1.0.0
  getIdentifierVersion :: Identifier -> Maybe String
  getIdentifierVersion = identifierVersion

  -- | Set the version of an identifier. I recommend that you do not use this
  -- function.
  --
  -- @since 0.1.0.0
  setIdentifierVersion :: Maybe String -> Identifier -> Identifier
  setIdentifierVersion v i = i { identifierVersion = v }
