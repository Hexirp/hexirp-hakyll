--------------------------------------------------------------------------------
-- | An identifier is a type used to uniquely identify an item. An identifier is
-- conceptually similar to a file path. Examples of identifiers are:
--
-- * @posts/foo.markdown@
--
-- * @index@
--
-- * @error/404@
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hexyll.Core.Identifier
    ( Identifier
    , fromFilePath
    , toFilePath
    , getIdentVersion
    , setIdentVersion
    ) where

import Prelude
import Control.Monad (mzero)
import Path hiding (toFilePath)
import qualified Path

--------------------------------------------------------------------------------
import           Control.DeepSeq     (NFData (..))


--------------------------------------------------------------------------------
import           Data.Binary         (Binary (..))
import           Data.Typeable       (Typeable)
import           GHC.Exts            (IsString, fromString)


--------------------------------------------------------------------------------
data Identifier = Identifier
    { identifierVersion :: Maybe String
    , identifierPath    :: Path Rel File
    } deriving (Eq, Ord, Typeable)


--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
instance IsString Identifier where
    fromString = fromFilePath


--------------------------------------------------------------------------------
instance NFData Identifier where
    rnf (Identifier v p) = rnf v `seq` rnf p `seq` ()


--------------------------------------------------------------------------------
instance Show Identifier where
    show i = case identifierVersion i of
        Nothing -> toFilePath i
        Just v  -> toFilePath i ++ " (" ++ v ++ ")"


--------------------------------------------------------------------------------
-- | Parse an identifier from a string
fromFilePath :: FilePath -> Identifier
fromFilePath s = case parseRelFile s of
  Nothing -> error "Identifier.fromFilePath: It's not a relative path to file."
  Just p -> Identifier Nothing p


--------------------------------------------------------------------------------
-- | Convert an identifier to a relative 'FilePath'
toFilePath :: Identifier -> FilePath
toFilePath = Path.toFilePath . identifierPath


getIdentVersion :: Identifier -> Maybe String
getIdentVersion = identifierVersion

--------------------------------------------------------------------------------
setIdentVersion :: Maybe String -> Identifier -> Identifier
setIdentVersion v i = i { identifierVersion = v }
