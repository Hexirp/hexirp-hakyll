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
import Hexyll.Core.Identifier.OldPattern
import Hexyll.Core.Dependencies


--------------------------------------------------------------------------------
newtype Metadata = Metadata
  { unMetadata :: Yaml.Object
  } deriving ( Eq, Show, Typeable )


--------------------------------------------------------------------------------
lookupString :: String -> Metadata -> Maybe String
lookupString key meta = HMS.lookup (T.pack key) meta >>= Yaml.toString


--------------------------------------------------------------------------------
lookupStringList :: String -> Metadata -> Maybe [String]
lookupStringList key meta =
    HMS.lookup (T.pack key) meta >>= Yaml.toList >>= mapM Yaml.toString


--------------------------------------------------------------------------------
class Monad m => MonadMetadata m where
    getMetadata    :: Identifier -> m Metadata
    getMatches     :: Pattern -> m [Identifier]

    getAllMetadata :: Pattern -> m [(Identifier, Metadata)]
    getAllMetadata pattern = do
        matches' <- getMatches pattern
        forM matches' $ \id' -> do
            metadata <- getMetadata id'
            return (id', metadata)


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


--------------------------------------------------------------------------------
makePatternDependency :: MonadMetadata m => Pattern -> m Dependency
makePatternDependency pattern = do
    matches' <- getMatches pattern
    return $ PatternDependency (toNew pattern) (S.fromList matches')


--------------------------------------------------------------------------------
-- | Newtype wrapper for serialization.
newtype BinaryMetadata = BinaryMetadata
    {unBinaryMetadata :: Metadata}


instance Binary BinaryMetadata where
    put (BinaryMetadata obj) = put (BinaryYaml $ Yaml.Object obj)
    get = do
        BinaryYaml (Yaml.Object obj) <- get
        return $ BinaryMetadata obj


