module Hexyll.Core.Log where

  import Prelude

  import Data.Typeable (Typeable)

  import Control.Monad.IO.Class     (MonadIO, liftIO)
  import Control.Monad.Reader.Class (MonadReader (ask))

  import Lens.Micro (Lens', view)

  data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelFatal
    deriving ( Eq, Ord, Enum, Bounded, Show, Typeable )

  type LogMessage = String

  newtype LogEnv = LogEnv
    { logFunc :: LogLevel -> LogMessage -> IO ()
    } deriving ( Typeable )

  sqLogEnv :: (LogLevel -> LogMessage -> IO ()) -> LogEnv
  sqLogEnv f = f `seq` LogEnv f

  class HasLogEnv env where
    logEnvL :: Lens' env LogEnv

  instance HasLogEnv LogEnv where
    logEnvL = id

  logGeneric
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogLevel
    -> LogMessage
    -> m ()
  logGeneric ll lm = do
    env <- ask
    liftIO $ logFunc (view logEnvL env) ll lm

  logDebug
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogMessage
    -> m ()
  logDebug lm = logGeneric LevelDebug lm

  logInfo
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogMessage
    -> m ()
  logInfo lm = logGeneric LevelInfo lm

  logWarn
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogMessage
    -> m ()
  logWarn lm = logGeneric LevelWarn lm

  logError
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogMessage
    -> m ()
  logError lm = logGeneric LevelError lm

  logFatal
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => LogMessage
    -> m ()
  logFatal lm = logGeneric LevelFatal lm
