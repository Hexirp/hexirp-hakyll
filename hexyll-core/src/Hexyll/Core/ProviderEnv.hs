module Hexyll.Core.ProviderEnv where

  import Prelude

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import qualified Data.Set as S

  import Path

  import Hexyll.Core.StoreEnv
  import Hexyll.Core.Provider

  type MTime = ModificationTime

  data ProviderEnv = ProviderEnv
    { providerGetAllPath
        :: StoreEnv -> IO (S.Set (Path Rel File))
    , providerGetMTimeDelay
        :: StoreEnv -> Path Rel File -> IO (Maybe (ProviderLoad IO MTime))
    , providerGetBodyDelay
        :: StoreEnv -> Path Rel File -> IO (Maybe (ProviderLoad IO Body))
    , providerStore
        :: StoreEnv
    }

  class HasProviderEnv env where
    providerEnvL :: Lens' env ProviderEnv

  instance HasProviderEnv ProviderEnv where
    providerEnvL = id

  getAllPathE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => m (S.Set (Path Rel File))
  getAllPathE = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        providerGetAllPath providerEnv (providerStore providerEnv)

  getModificationTimeDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))
  getModificationTimeDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetMTimeDelay providerEnv (providerStore providerEnv) p

  getBodyDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m Body))
  getBodyDelayE p = do
    env <- ask
    let providerEnv = view providerEnvL env in
      liftIO $
        fmap (fmap (mapProviderLoad liftIO)) $
          providerGetBodyDelay providerEnv (providerStore providerEnv)  p
