{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}

module Hexyll.Core.StoreEnv where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import qualified Data.ByteArray       as BA
  import qualified Crypto.Hash          as CH

  import Numeric ( showHex )

  import Data.Word ( Word8 )

  import qualified Data.ByteString      as B
  import qualified Data.Text            as T
  import qualified Data.Text.Encoding   as T

  import Path

  import System.Directory ( createDirectoryIfMissing )

  import Hexyll.Core.Store

  data StoreEnv = StoreEnv
    { storeSave :: !(StoreKey -> StoreValue -> IO ())
    , storeLoadDelay :: !(StoreKey -> IO (Maybe (StoreLoad IO)))
    } deriving Typeable

  class HasStoreEnv env where
    storeEnvL :: Lens' env StoreEnv

  saveE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> StoreValue
    -> m ()
  saveE sk sv = do
    env <- ask
    liftIO $ storeSave (view storeEnvL env) sk sv

  loadDelayE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> m (Maybe (StoreLoad m))
  loadDelayE sk = do
    env <- ask
    liftIO $ fmap (fmap (mapStoreLoad liftIO)) $
      storeLoadDelay (view storeEnvL env) sk

  data StoreOption = StoreOption
    { storeLocation :: !(Path Rel Dir)
    , storeInMemory :: !Bool
    } deriving (Eq, Ord, Show, Typeable)

  newStoreEnv :: StoreOption -> IO StoreEnv
  newStoreEnv (StoreOption sl si) = if si
    then newStoreEnvInMemory sl
    else newStoreEnvNoMemory sl

  newStoreEnvInMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvInMemory = undefined

  newStoreEnvNoMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvNoMemory dir = do
    createDirectoryIfMissing True (toFilePath dir)
    return $ StoreEnv
      { storeSave = newStoreEnvNoMemory_save dir
      , storeLoadDelay = newStoreEnvNoMemory_loadDelay dir
      }
  
  newStoreEnvNoMemory_save
    :: Path Rel Dir -> StoreKey -> StoreValue -> IO ()
  newStoreEnvNoMemory_save = undefined

  newStoreEnvNoMemory_loadDelay
    :: Path Rel Dir -> StoreKey -> IO (Maybe (StoreLoad IO))
  newStoreEnvNoMemory_loadDelay = undefined

  withStorePath
    :: Path Rel Dir -> StoreKey -> (String -> Path Rel File -> IO a) -> IO a
  withStorePath dir key f =
    let
      keyHash = hashStoreKey key
      mpath = (dir </>) <$> parseRelFile keyHash
    in case mpath of
      Left e -> error $ unlines
        [ "withStoreKey: Something wrong happened."
        , "withStoreKey: " ++ show (show e)
        ]
      Right path -> f keyHash path

  hashStoreKey :: StoreKey -> String
  hashStoreKey sk =
    let
      --
      s :: String
      s = sk
      t :: T.Text
      t = T.pack s
      bUTF8 :: B.ByteString
      bUTF8 = T.encodeUtf8 t
      bHash :: B.ByteString
      bHash = hashMD5 bUTF8
      sHash :: [Word8]
      sHash = B.unpack bHash
      sHex :: String
      sHex = toHex sHash
      --
      hashMD5 :: B.ByteString -> B.ByteString
      hashMD5 x =
        let
          digest :: CH.Digest CH.MD5
          digest = CH.hash x
          bytes :: B.ByteString
          bytes = BA.convert digest
        in
          bytes
      --
      toHex :: [Word8] -> String
      toHex [] = ""
      toHex (xv : xs) = case showHex xv [] of
        {      c0 : [] -> '0' : c0 : toHex xs
        ; c1 : c0 : [] ->  c1 : c0 : toHex xs
        ; _            -> error "toHex: Impossible case"
        }
    in
      sHex
