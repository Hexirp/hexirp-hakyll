-- |
-- Module:      Hexyll.Core.Identifier
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Identifier', a type used to uniquely identify an item.
-- An identifier is conceptually similar to a file path. Examples of
-- identifiers are:
--
-- * @posts/foo.markdown@
-- * @index@
-- * @error/404@
--
-- A 'Identifier' value can have its version. The information about version is
-- used inside the library.
--
-- @since 0.1.0.0
module Hexyll.Core.Identifier where

  import Prelude

  import Control.Monad.Catch (MonadThrow)

  import Control.DeepSeq (NFData (..))
  import Data.Binary     (Binary (..))
  import Data.String     (IsString, fromString)
  import Data.Typeable   (Typeable)

  import Path

  -- | A type used to uniquely identify an item.
  --
  -- It is similar to 'FilePath'. But, a 'Identifier' value can have its
  -- version. The information about version is used inside the library.
  --
  -- @since 0.1.0.0
  data Identifier = Identifier
    { identifierPath    :: !(Path Rel File)
    , identifierVersion :: !(Maybe String)
    } deriving (Eq, Ord, Typeable)

  -- | @since 0.1.0.0
  instance Binary Identifier where
    put (Identifier p v) = do
      put (toFilePath p)
      put v
    get = do
      v <- get
      p <- do
        s <- get
        case parseRelFile s of
          Nothing -> error "Data.Binary.get: Invalid Identifier"
          Just p -> return p
      return $ Identifier p v

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

  -- | @since 0.1.0.0
  instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> fromIdentifierToFilePath i
        Just v  -> fromIdentifierToFilePath i ++ " (" ++ v ++ ")"

  -- | Make an identifier from a path.
  --
  -- @since 0.1.0.0
  fromPath :: Path Rel File -> Identifier
  fromPath p = Identifier p Nothing

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

  -- | Convert an identifier to a path.
  --
  -- @since 0.1.0.0
  fromIdentifierToPath :: Identifier -> Path Rel File
  fromIdentifierToPath = identifierPath

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
