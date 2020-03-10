module Hexyll.Core.Metadata where

  import Data.Binary   ( Binary (..) )
  import Data.Typeable ( Typeable )
  import Data.String ( IsString (..) )

  import qualified Data.HashMap.Strict as HM
  import qualified Data.Text           as T
  import qualified Data.Yaml           as Yaml
  import qualified Data.Yaml.Hexyll    as Yaml

  import Control.Monad ( forM )

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern hiding ( Pattern )

  -- | A type of patterns for 'MonadMetadata'.
  --
  -- @since 0.1.0.0
  newtype Pattern = Pattern
    { unPattern :: PatternExpr
    } deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString s = Pattern $ fromString s

  class Monad m => MonadUniverse m where

    getMatches :: Pattern -> m [Identifier]

    countUniverse :: m Int
    countUniverse = length <$> getMatches (Pattern everything)

  newtype Metadata = Metadata
    { unMetadata :: Yaml.Object
    } deriving ( Eq, Show, Typeable )

  instance Semigroup Metadata where
    Metadata x <> Metadata y = Metadata (x <> y)

  instance Monoid Metadata where
    mempty = Metadata mempty

  instance Binary Metadata where
    put (Metadata x) = put $ Yaml.BinaryValue $ Yaml.Object x
    get = do
      Yaml.BinaryValue x' <- get
      case x' of
        Yaml.Object x ->
          return $ Metadata x
        _ ->
          error "Data.Binary.get: Invalid Metadata"

  instance Yaml.ToJSON Metadata where
    toJSON (Metadata x) = Yaml.toJSON x

  instance Yaml.FromJSON Metadata where
    parseJSON v = Metadata <$> Yaml.parseJSON v

  lookupString :: String -> Metadata -> Maybe String
  lookupString key (Metadata meta) =
    HM.lookup (T.pack key) meta >>= Yaml.toString

  lookupStringList :: String -> Metadata -> Maybe [String]
  lookupStringList key (Metadata meta) =
      HM.lookup (T.pack key) meta >>= Yaml.toList >>= mapM Yaml.toString

  class MonadUniverse m => MonadMetadata m where

    getMetadata :: Identifier -> m Metadata

    getAllMetadata :: Pattern -> m [(Identifier, Metadata)]
    getAllMetadata p = do
      is <- getMatches p
      forM is $ \i -> do
        m <- getMetadata i
        return (i, m)

  getMetadataField :: MonadMetadata m => Identifier -> String -> m (Maybe String)
  getMetadataField identifier key = do
      metadata <- getMetadata identifier
      return $ lookupString key metadata

  -- | Version of 'getMetadataField' which throws an error if the field does not
  -- exist.
  getMetadataField' :: MonadMetadata m => Identifier -> String -> m String
  getMetadataField' identifier key = do
      field <- getMetadataField identifier key
      case field of
          Just v  -> return v
          Nothing -> fail $ "Hexyll.Core.Metadata.getMetadataField': " ++
              "Item " ++ show identifier ++ " has no metadata field " ++ show key
