--------------------------------------------------------------------------------
module Hexyll.Core.Provider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless)
import           Hexyll.Core.Identifier
import           Hexyll.Core.Metadata
import           Hexyll.Core.Provider.Internal
import           Hexyll.Core.Provider.Metadata
import qualified Hexyll.Core.Store             as Store


--------------------------------------------------------------------------------
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata p r
    | not (resourceExists p r) = return mempty
    | otherwise                = do
        -- TODO keep time in md cache
        load p r
        Store.Found (BinaryMetadata md) <- Store.get (providerStore p)
            [name, toFilePath r, "metadata"]
        return md


--------------------------------------------------------------------------------
resourceBody :: Provider -> Identifier -> IO String
resourceBody p r = do
    load p r
    Store.Found bd <- Store.get (providerStore p)
        [name, toFilePath r, "body"]
    maybe (resourceString p r) return bd


--------------------------------------------------------------------------------
resourceInvalidateMetadataCache :: Provider -> Identifier -> IO ()
resourceInvalidateMetadataCache p r = do
    Store.delete (providerStore p) [name, toFilePath r, "metadata"]
    Store.delete (providerStore p) [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
load :: Provider -> Identifier -> IO ()
load p r = do
    mmof <- Store.isMember store mdk
    unless mmof $ do
        (md, body) <- loadMetadata p r
        Store.set store mdk (BinaryMetadata md)
        Store.set store bk  body
  where
    store = providerStore p
    mdk   = [name, toFilePath r, "metadata"]
    bk    = [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hexyll.Core.Resource.Provider.MetadataCache"