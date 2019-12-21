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

  data Identifier = Identifier
    { identifierVersion :: Maybe String
    , identifierPath    :: Path Rel File
    } deriving (Eq, Ord, Typeable)

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

  instance IsString Identifier where
    fromString = fromFilePath

  instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()

  instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> toFilePath i
        Just v  -> toFilePath i ++ " (" ++ v ++ ")"

  -- | Parse an identifier from a string
  fromFilePath :: FilePath -> Identifier
  fromFilePath s = case parseRelFile s of
    Nothing -> error "Identifier.fromFilePath: It's not a relative path to file."
    Just p -> Identifier Nothing p

  -- | Convert an identifier to a relative 'FilePath'
  toFilePath :: Identifier -> FilePath
  toFilePath = Path.toFilePath . identifierPath

  getIdentVersion :: Identifier -> Maybe String
  getIdentVersion = identifierVersion

  setIdentVersion :: Maybe String -> Identifier -> Identifier
  setIdentVersion v i = i { identifierVersion = v }
