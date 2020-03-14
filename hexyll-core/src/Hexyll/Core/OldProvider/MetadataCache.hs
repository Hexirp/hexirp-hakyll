--------------------------------------------------------------------------------
module Hexyll.Core.OldProvider.MetadataCache
    ( resourceMetadata
    , resourceBody
    , resourceInvalidateMetadataCache
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                 (unless)
import           Hexyll.Core.Identifier
import           Hexyll.Core.Metadata
import           Hexyll.Core.OldProvider.Internal
import           Hexyll.Core.OldProvider.Metadata
import qualified Hexyll.Core.OldStore          as Store


--------------------------------------------------------------------------------
resourceMetadata :: Provider -> Identifier -> IO Metadata
resourceMetadata p r
    | not (resourceExists p r) = return mempty
    | otherwise                = do
        -- TODO keep time in md cache
        load p r
        Store.Found md <- Store.get (providerStore p)
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
        Store.set store mdk md
        Store.set store bk  body
  where
    store = providerStore p
    mdk   = [name, toFilePath r, "metadata"]
    bk    = [name, toFilePath r, "body"]


--------------------------------------------------------------------------------
name :: String
name = "Hexyll.Core.Resource.Provider.MetadataCache"
