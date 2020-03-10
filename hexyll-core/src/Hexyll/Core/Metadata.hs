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

  -- | Monads that has a set of identifiers and can search for them.
  --
  -- @since 0.1.0.0
  class Monad m => MonadUniverse m where

    -- | Get identifiers that matches the pattern.
    --
    -- @since 0.1.0.0
    getMatches :: Pattern -> m [Identifier]

    -- | Get all identifiers.
    --
    -- @since 0.1.0.0
    getAllIdentifier :: m [Identifier]
    getAllIdentifier = getMatches (Pattern everything)

    -- | Count the number of all identifiers.
    --
    -- @since 0.1.0.0
    countUniverse :: m Int
    countUniverse = length <$> getMatches (Pattern everything)

  -- | Metadata associated with identifiers.
  --
  -- @since 0.1.0.0
  newtype Metadata = Metadata
    { unMetadata :: Yaml.Object
    } deriving ( Eq, Show, Typeable )

  -- | @since 0.1.0.0
  instance Semigroup Metadata where
    Metadata x <> Metadata y = Metadata (x <> y)

  -- | @since 0.1.0.0
  instance Monoid Metadata where
    mempty = Metadata mempty

  -- | @since 0.1.0.0
  instance Binary Metadata where
    put (Metadata x) = put $ Yaml.BinaryValue $ Yaml.Object x
    get = do
      Yaml.BinaryValue x' <- get
      case x' of
        Yaml.Object x ->
          return $ Metadata x
        _ ->
          error "Data.Binary.get: Invalid Metadata"

  -- @since 0.1.0.0
  instance Yaml.ToJSON Metadata where
    toJSON (Metadata x) = Yaml.toJSON x

  -- @since 0.1.0.0
  instance Yaml.FromJSON Metadata where
    parseJSON v = Metadata <$> Yaml.parseJSON v

  -- Look up the field corresponding to the key and convert it to a string.
  -- Returns @Nothing@ if the field cannot be converted to a string.
  --
  -- See 'Yaml.toString'.
  --
  -- @since 0.1.0.0
  lookupString :: String -> Metadata -> Maybe String
  lookupString key (Metadata meta) =
    HM.lookup (T.pack key) meta >>= Yaml.toString

  -- Look up the field corresponding to the key and convert it to a list of
  -- strings. If there is at least one failure, Nothing is returned.
  --
  -- See 'Yaml.toList' and 'Yaml.toString'.
  --
  -- @since 0.1.0.0
  lookupStringList :: String -> Metadata -> Maybe [String]
  lookupStringList key (Metadata meta) =
    HM.lookup (T.pack key) meta >>= Yaml.toList >>= mapM Yaml.toString

  -- | Monads that can get metadata.
  --
  -- @since 0.1.0.0
  class MonadUniverse m => MonadMetadata m where

    -- | Get the metadata corresponding to the identifier.
    getMetadata :: Identifier -> m Metadata

    -- | Get metadata that matches the pattern in key-value format.
    getMetadataMatches :: Pattern -> m [(Identifier, Metadata)]
    getMetadataMatches p = do
      is <- getMatches p
      forM is $ \i -> do
        m <- getMetadata i
        return (i, m)

  getMetadataField
    :: MonadMetadata m
    => Identifier -> String -> m (Maybe String)
  getMetadataField identifier key = do
    metadata <- getMetadata identifier
    return $ lookupString key metadata
