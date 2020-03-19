{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.ProviderEnv
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides an environment for handling a provider.
--
-- @since 0.1.0.0
module Hexyll.Core.ProviderEnv where

  import Prelude

  import Data.Coerce ( coerce )

  import Control.Monad ( forM )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens', lens )
  import Lens.Micro.Extras ( view )

  import qualified Data.ByteString.Lazy as BL

  import qualified Data.Set as S
  import qualified Data.Map as M

  import Data.Time        ( UTCTime )
  import Data.Time.Hexyll

  import Path
  import System.Directory        ( getModificationTime )
  import System.Directory.Hexyll ( listDirectoryRecursive )

  import Hexyll.Core.Store
  import Hexyll.Core.StoreEnv
  import Hexyll.Core.Provider

  -- | The type of environment for handling a provider.
  --
  -- @since 0.1.0.0
  data ProviderEnv = ProviderEnv
    { providerGetAllPath
        :: !(StoreEnv -> IO (S.Set Resource))
    , providerGetMTimeDelay
        :: !(StoreEnv -> Resource -> IO (Maybe (ProviderLoad IO MTime)))
    , providerGetBodyDelay
        :: !(StoreEnv -> Resource -> IO (Maybe (ProviderLoad IO Body)))
    , providerStore
        :: !StoreEnv
    }

  -- | @since 0.1.0.0
  instance HasStoreEnv ProviderEnv where
    storeEnvL =
      lens providerStore (\env store -> env { providerStore = store })

  -- | Environment values with functions handling a provider.
  --
  -- @since 0.1.0.0
  class HasProviderEnv env where
    providerEnvL :: Lens' env ProviderEnv

  -- | @since 0.1.0.0
  instance HasProviderEnv ProviderEnv where
    providerEnvL = id

  -- | Get all paths.
  --
  -- @since 0.1.0.0
  getAllPathE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => m (S.Set Resource)
  getAllPathE = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        providerGetAllPath providerEnv (providerStore providerEnv)

  -- | Get the modification time of a file lazily.
  --
  -- @since 0.1.0.0
  getMTimeDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Resource -> m (Maybe (ProviderLoad m MTime))
  getMTimeDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetMTimeDelay providerEnv (providerStore providerEnv) p

  -- | Get the body of a file lazily.
  --
  -- @since 0.1.0.0
  getBodyDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Resource -> m (Maybe (ProviderLoad m Body))
  getBodyDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetBodyDelay providerEnv (providerStore providerEnv) p

  data ProviderOption = ProviderOption
    { providerLocation :: !(Path Rel Dir)
    , providerIgnore :: !(Path Rel File -> Bool)
    }

  newProviderEnv :: ProviderOption -> StoreEnv -> IO ProviderEnv
  newProviderEnv po se = do
    ps <- do
      psr <- listDirectoryRecursive $ providerLocation po
      return $ S.fromList $ fmap Resource $ filter (providerIgnore po) psr
    ri <- do
      tsn <- fmap M.fromList $ forM (S.toList ps) $ \p -> do
        t <- getModificationTime $ toFilePath $ unResource p
        return (p, t)
      msl <- storeLoadDelay se newProviderEnv_key
      tso <- case msl of
        Nothing -> return M.empty
        Just sl -> do
          etso' <- runStoreLoad sl
          case etso' of
            Left e -> error $ "newProviderEnv: " ++ show e
            Right tso' -> return $
              let
                coerce' :: M.Map Resource BinaryTime -> M.Map Resource UTCTime
                coerce' = coerce
              in
                coerce' tso'
      storeSave se newProviderEnv_key $ MkStoreValue $
        let
          coerce' :: M.Map Resource UTCTime -> M.Map Resource BinaryTime
          coerce' = coerce
        in
          coerce' tsn
      return $ M.mapWithKey (\p t -> MTime t (M.lookup p tso)) tsn
    return $ ProviderEnv
      { providerGetAllPath = newProviderEnv_getAllPath ri
      , providerGetMTimeDelay = newProviderEnv_getMTimeDelay ri
      , providerGetBodyDelay = newProviderEnv_getBodyDelay ri
      , providerStore = se
      }

  newProviderEnv_key :: String
  newProviderEnv_key = unwords ["Hexyll.Core.ProviderEnv", "Time"]

  newProviderEnv_getAllPath
    :: M.Map Resource MTime
    -> StoreEnv -> IO (S.Set Resource)
  newProviderEnv_getAllPath ri _ = return $ M.keysSet ri

  newProviderEnv_getMTimeDelay
    :: M.Map Resource MTime
    -> StoreEnv -> Resource -> IO (Maybe (ProviderLoad IO MTime))
  newProviderEnv_getMTimeDelay ri _ p =
    return $ fmap (ProviderLoad . return) $ M.lookup p ri

  newProviderEnv_getBodyDelay
    :: M.Map Resource MTime
    -> StoreEnv -> Resource -> IO (Maybe (ProviderLoad IO Body))
  newProviderEnv_getBodyDelay ri _ p =
    if M.member p ri
      then return $ Just $ ProviderLoad $ do
        bd <- BL.readFile $ toFilePath $ unResource p
        return $ Body bd
      else return Nothing
