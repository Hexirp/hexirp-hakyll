module Hexyll.Core.ProviderEnv where

  import Prelude

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import qualified Data.Set as S

  import Path

  import Hexyll.Core.Provider

  data ProviderEnv = ProviderEnv
    { providerGetAllPath :: IO (S.Set (Path Rel File))
    , providerGetModificationTimeDelay
        :: Path Rel File -> IO (Maybe (ProviderLoad IO ModificationTime))
    , providerGetBodyDelay
        :: Path Rel File -> IO (Maybe (ProviderLoad IO Body))
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
    liftIO $ providerGetAllPath (view providerEnvL env)

  getModificationTimeDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m ModificationTime))
  getModificationTimeDelayE p = do
    env <- ask
    liftIO $ fmap (fmap (mapProviderLoad liftIO)) $
      providerGetModificationTimeDelay (view providerEnvL env) p

  getBodyDelayE
    :: (MonadIO m, MonadReader env m, HasProviderEnv env)
    => Path Rel File -> m (Maybe (ProviderLoad m Body))
  getBodyDelayE p = do
    env <- ask
    liftIO $ fmap (fmap (mapProviderLoad liftIO)) $
      providerGetBodyDelay (view providerEnvL env) p
