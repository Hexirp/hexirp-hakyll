{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
module Hexyll.Core.Identifier
  ( Identifier
  , fromFilePath
  , toFilePath
  , getIdentVersion
  , setIdentVersion
  ) where

  import Prelude

  import Control.Monad   (mzero)
  import Control.DeepSeq (NFData (..))

  import Data.String   (IsString, fromString)
  import Data.Typeable (Typeable)

  import Data.Binary (Binary (..))

  import qualified Path
  import           Path hiding (toFilePath)

  -- | A type used to uniquely identify an item.
  --
  -- It is similar to 'FilePath'. But, a 'Identifier' value can have its
  -- version. The information about version is used inside the library.
  --
  -- @since 0.1.0.0
  data Identifier = Identifier
    { identifierVersion :: Maybe String
    , identifierPath    :: Path Rel File
    } deriving (Eq, Ord, Typeable)

  -- @since 0.1.0.0
  instance Binary Identifier where
    put (Identifier v p) = do
      put v
      put $ Path.toFilePath p
    get = do
      v <- get
      p <- do
        s <- get
        case parseRelFile s of
          Nothing -> mzero
          Just p -> return p
      return $ Identifier v p

  -- @since 0.1.0.0
  instance IsString Identifier where
    fromString = fromFilePath

  -- @since 0.1.0.0
  instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()

  -- @since 0.1.0.0
  instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> toFilePath i
        Just v  -> toFilePath i ++ " (" ++ v ++ ")"

  -- | Parse an identifier from a string. The string should be a relative path
  -- to file.
  --
  -- @since 0.1.0.0
  fromFilePath :: FilePath -> Identifier
  fromFilePath s = case parseRelFile s of
    Nothing -> error "Identifier.fromFilePath: It's not a relative path to file."
    Just p -> Identifier Nothing p

  -- | Convert an identifier to a relative 'FilePath'.
  --
  -- @since 0.1.0.0
  toFilePath :: Identifier -> FilePath
  toFilePath = Path.toFilePath . identifierPath

  -- | Get the version of an identifier. I recommend that you do not use this
  -- function.
  --
  -- @since 0.1.0.0
  getIdentVersion :: Identifier -> Maybe String
  getIdentVersion = identifierVersion

  -- | Set the version of an identifier. I recommend that you do not use this
  -- function.
  --
  -- @since 0.1.0.0
  setIdentVersion :: Maybe String -> Identifier -> Identifier
  setIdentVersion v i = i { identifierVersion = v }
