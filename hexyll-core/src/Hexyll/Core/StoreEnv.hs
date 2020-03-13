{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.StoreEnv
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides an environment for handling a store.
--
-- @since 0.1.0.0
module Hexyll.Core.StoreEnv where

  import Prelude

  import Data.Typeable ( Typeable, typeOf, typeRep, cast )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import Control.DeepSeq ( deepseq )

  import Numeric ( showHex )

  import Data.Binary ( encodeFile, decodeOrFail )
  import Data.Word   ( Word8 )

  import qualified Data.ByteString      as B
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.Text            as T
  import qualified Data.Text.Encoding   as T

  import qualified Data.ByteArray as BA
  import qualified Crypto.Hash    as CH

  import Path
  import System.Directory ( createDirectoryIfMissing, doesFileExist )
  import System.IO        ( withFile, IOMode (..) )

  import System.IO.Error  ( modifyIOError, ioeSetLocation, ioeSetFileName )

  import qualified Data.Cache.LRU.IO as Lru

  import Hexyll.Core.Store

  -- | The type of environment for handling a store.
  --
  -- @since 0.1.0.0
  data StoreEnv = StoreEnv
    { storeSave :: !(StoreKey -> StoreValue -> IO ())
    , storeLoadDelay :: !(StoreKey -> IO (Maybe (StoreLoad IO)))
    } deriving Typeable

  -- | Environment values with functions handling a store.
  --
  -- @since 0.1.0.0
  class HasStoreEnv env where
    storeEnvL :: Lens' env StoreEnv

  -- | Save a value with a key.
  --
  -- @since 0.1.0.0
  saveE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> StoreValue
    -> m ()
  saveE sk sv = do
    env <- ask
    liftIO $ storeSave (view storeEnvL env) sk sv

  -- | Load a value lazily.
  --
  -- @since 0.1.0.0
  loadDelayE
    :: (MonadIO m, MonadReader env m, HasStoreEnv env)
    => StoreKey
    -> m (Maybe (StoreLoad m))
  loadDelayE sk = do
    env <- ask
    liftIO $ fmap (fmap (mapStoreLoad liftIO)) $
      storeLoadDelay (view storeEnvL env) sk

  -- | The option for 'newStoreEnv'
  --
  -- @since 0.1.0.0
  data StoreOption = StoreOption
    { storeLocation :: !(Path Rel Dir)
    , storeInMemory :: !Bool
    } deriving (Eq, Ord, Show, Typeable)

  -- | Made a new 'StoreEnv'.
  --
  -- @since 0.1.0.0
  newStoreEnv :: StoreOption -> IO StoreEnv
  newStoreEnv (StoreOption sl si) = if si
    then newStoreEnvInMemory sl
    else newStoreEnvNoMemory sl

  newStoreEnvInMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvInMemory dir = do
    createDirectoryIfMissing True (toFilePath dir)
    cache <- Lru.newAtomicLRU storeCacheSize
    return $ StoreEnv
      { storeSave = newStoreEnvInMemory_save dir cache
      , storeLoadDelay = newStoreEnvInMemory_loadDelay dir cache
      }

  -- This magic number comes from https://github.com/Hexirp/hexirp-hakyll/blob/6732588a8ea77651201d179beab0ac868067e01f/hexyll-core/src/Hexyll/Core/OldStore.hs#L87 .
  storeCacheSize :: Maybe Integer
  storeCacheSize = Just 512

  newStoreEnvInMemory_save
    :: Path Rel Dir -> Lru.AtomicLRU (Path Rel File) StoreValue
    -> StoreKey -> StoreValue -> IO ()
  newStoreEnvInMemory_save dir cache key value =
    withStorePath dir key $ \path -> do
      case value of
        MkStoreValue x ->
          let
            handle e = e
              `ioeSetFileName` (show path ++ " for " ++ key)
              `ioeSetLocation` "newStoreEnvInMemory_save"
          in
            modifyIOError handle $ encodeFile (toFilePath path) x
      Lru.insert path value cache

  newStoreEnvInMemory_loadDelay
    :: Path Rel Dir -> Lru.AtomicLRU (Path Rel File) StoreValue
    -> StoreKey -> IO (Maybe (StoreLoad IO))
  newStoreEnvInMemory_loadDelay dir cache key =
    withStorePath dir key $ \path -> do
      mv <- Lru.lookup path cache
      case mv of
        Nothing -> do
          exists <- doesFileExist $ toFilePath path
          if exists
            then return $ Just $ StoreLoad $
              let
                handle e = e
                  `ioeSetFileName` (show path ++ " for " ++ key)
                  `ioeSetLocation` "newStoreEnvNoMemory_loadDelay"
              in
                modifyIOError handle $
                  withFile (toFilePath path) ReadMode $ \h -> do
                    c <- BL.hGetContents h
                    c `deepseq` case decodeOrFail c of
                      Left  (_, _, s) ->
                        return $ Left (DecodeError (StoreDecodeError s))
                      Right (_, _, x) ->
                        return $ Right x
            else return Nothing
        Just v -> return $ Just $ StoreLoad $ case v of
          MkStoreValue c -> let mx = cast c in case mx of
            Nothing ->
              return $ Left $ TypeCastError $ StoreTypeCastError
                { storeExpected = typeRep mx
                , storeActual = typeOf c
                }
            Just x ->
              return $ Right x

  newStoreEnvNoMemory :: Path Rel Dir -> IO StoreEnv
  newStoreEnvNoMemory dir = do
    createDirectoryIfMissing True (toFilePath dir)
    return $ StoreEnv
      { storeSave = newStoreEnvNoMemory_save dir
      , storeLoadDelay = newStoreEnvNoMemory_loadDelay dir
      }
  
  newStoreEnvNoMemory_save
    :: Path Rel Dir -> StoreKey -> StoreValue -> IO ()
  newStoreEnvNoMemory_save dir key value =
    withStorePath dir key $ \path ->
      case value of
        MkStoreValue x ->
          let
            handle e = e
              `ioeSetFileName` (show path ++ " for " ++ key)
              `ioeSetLocation` "newStoreEnvNoMemory_save"
          in
            modifyIOError handle $ encodeFile (toFilePath path) x

  newStoreEnvNoMemory_loadDelay
    :: Path Rel Dir -> StoreKey -> IO (Maybe (StoreLoad IO))
  newStoreEnvNoMemory_loadDelay dir key =
    withStorePath dir key $ \path -> do
      exists <- doesFileExist $ toFilePath path
      if exists
        then return $ Just $ StoreLoad $
          let
            handle e = e
              `ioeSetFileName` (show path ++ " for " ++ key)
              `ioeSetLocation` "newStoreEnvNoMemory_loadDelay"
          in
            modifyIOError handle $
              withFile (toFilePath path) ReadMode $ \h -> do
                c <- BL.hGetContents h
                c `deepseq` case decodeOrFail c of
                  Left  (_, _, s) ->
                    return $ Left (DecodeError (StoreDecodeError s))
                  Right (_, _, x) ->
                    return $ Right x
        else return Nothing

  withStorePath
    :: Path Rel Dir -> StoreKey -> (Path Rel File -> IO a) -> IO a
  withStorePath dir key f =
    let
      keyHash = hashStoreKey key
      mpath = (dir </>) <$> parseRelFile keyHash
    in case mpath of
      Left e -> error $ unlines
        [ "withStorePath: Something wrong happened."
        , "withStorePath: " ++ show (show e)
        ]
      Right path -> f path

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
