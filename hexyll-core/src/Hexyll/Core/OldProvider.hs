--------------------------------------------------------------------------------
-- | This module provides an wrapper API around the file system which does some
-- caching.
module Hexyll.Core.OldProvider
    ( -- * Constructing resource providers
      Internal.Provider
    , newProvider

      -- * Querying resource properties
    , Internal.resourceList
    , Internal.resourceExists
    , Internal.resourceFilePath
    , Internal.resourceModified
    , Internal.resourceModificationTime

      -- * Access to raw resource content
    , Internal.resourceString
    , Internal.resourceLBS

      -- * Access to metadata and body content
    , Internal.resourceMetadata
    , Internal.resourceBody
    ) where


--------------------------------------------------------------------------------
import qualified Hexyll.Core.OldProvider.Internal      as Internal
import qualified Hexyll.Core.OldProvider.MetadataCache as Internal
import           Hexyll.Core.OldStore               (Store)


--------------------------------------------------------------------------------
-- | Create a resource provider
newProvider :: Store                   -- ^ Store to use
            -> (FilePath -> IO Bool)   -- ^ Should we ignore this file?
            -> FilePath                -- ^ Search directory
            -> IO Internal.Provider    -- ^ Resulting provider
newProvider store ignore directory = do
    -- Delete metadata cache where necessary
    p <- Internal.newProvider store ignore directory
    mapM_ (Internal.resourceInvalidateMetadataCache p) $
        filter (Internal.resourceModified p) $ Internal.resourceList p
    return p
