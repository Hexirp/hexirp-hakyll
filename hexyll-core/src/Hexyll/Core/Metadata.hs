--------------------------------------------------------------------------------
module Hexyll.Core.Metadata
    ( Metadata
    , lookupString
    , lookupStringList

    , MonadMetadata (..)
    , getMetadataField
    , getMetadataField'
    , makePatternDependency

    , BinaryMetadata (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                  (second)
import           Control.Monad                  (forM)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8, Get)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Data.Yaml                             as Yaml
import qualified Data.Yaml.Hexyll                      as Yaml

import Data.Typeable ( Typeable )

import Hexyll.Core.Identifier
import Hexyll.Core.Identifier.Pattern hiding ( Pattern )
import Hexyll.Core.Dependencies


--------------------------------------------------------------------------------
newtype Metadata = Metadata
  { unMetadata :: Yaml.Object
  } deriving ( Eq, Show, Typeable )

instance Binary Metadata where
  put (Metadata x) = put $ Yaml.BinaryValue $ Yaml.Object x
  get = do
    Yaml.BinaryValue x' <- get
    case x' of
      Yaml.Object x ->
        return $ Metadata x
      _ ->
        error "Data.Binary.get: Invalid Metadata"

--------------------------------------------------------------------------------
lookupString :: String -> Metadata -> Maybe String
lookupString key (Metadata meta) =
  HMS.lookup (T.pack key) meta >>= Yaml.toString


--------------------------------------------------------------------------------
lookupStringList :: String -> Metadata -> Maybe [String]
lookupStringList key (Metadata meta) =
    HMS.lookup (T.pack key) meta >>= Yaml.toList >>= mapM Yaml.toString


data Pattern = Pattern
  { unPattern :: PatternExpr
  } deriving ( Eq, Ord, Show, Typeable )

class Monad m => MonadMetadata m where

  getMetadata :: Identifier -> m Metadata
  getMatches  :: Pattern -> m [Identifier]

  getAllMetadata :: Pattern -> m [(Identifier, Metadata)]
  getAllMetadata p = do
    is <- getMatches p
    forM is $ \i -> do
      m <- getMetadata i
      return (i, m)


--------------------------------------------------------------------------------
getMetadataField :: MonadMetadata m => Identifier -> String -> m (Maybe String)
getMetadataField identifier key = do
    metadata <- getMetadata identifier
    return $ lookupString key metadata


--------------------------------------------------------------------------------
-- | Version of 'getMetadataField' which throws an error if the field does not
-- exist.
getMetadataField' :: MonadMetadata m => Identifier -> String -> m String
getMetadataField' identifier key = do
    field <- getMetadataField identifier key
    case field of
        Just v  -> return v
        Nothing -> fail $ "Hexyll.Core.Metadata.getMetadataField': " ++
            "Item " ++ show identifier ++ " has no metadata field " ++ show key


