{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module:      Hexyll.Core.Identifier.Internal
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   internal
-- Portability: portable
--
-- This is an internal module for "Hexyll.Core.Identifier".
--
-- @since 0.1.0.0
module Hexyll.Core.Identifier.Internal where

  import Prelude

  import Control.Monad       (mzero)
  import Control.Monad.Catch (MonadThrow)

  import Control.DeepSeq (NFData (..))
  import Data.Binary     (Binary (..))
  import Data.String     (IsString, fromString)
  import Data.Typeable   (Typeable)

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

  -- | @since 0.1.0.0
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

  -- | @since 0.1.0.0
  instance IsString Identifier where
    fromString s = case fromFilePath s of
      Left e -> error $ unlines
        [ "fromString @Identifier: It's not a relative path to file."
        , "fromString @Identifier: " ++ show (show e)
        ]
      Right i -> i

  -- | @since 0.1.0.0
  instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()

  -- | @since 0.1.0.0
  instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> toFilePath i
        Just v  -> toFilePath i ++ " (" ++ v ++ ")"

  -- | Parse an identifier from a string. The string should be a relative path
  -- to file.
  --
  -- @since 0.1.0.0
  fromFilePath :: MonadThrow m => FilePath -> m Identifier
  fromFilePath s = Identifier Nothing <$> parseRelFile s

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
