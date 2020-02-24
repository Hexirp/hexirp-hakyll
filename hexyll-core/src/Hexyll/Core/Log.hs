-- |
-- Module:      Hexyll.Core.Log
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: non-portable (multi-parameter type classes)
--
-- This module provides an environment for logging.
--
-- @since 0.1.0.0
module Hexyll.Core.Log where

  import Prelude

  import Data.Typeable (Typeable)

  import Control.Monad.IO.Class     (MonadIO, liftIO)
  import Control.Monad.Reader.Class (MonadReader (ask))

  import Lens.Micro        (Lens')
  import Lens.Micro.Extras (view)

  -- | The log level of a message in 'LogEnv'. This comes from apache log4j.
  --
  -- @since 0.1.0.0
  data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelFatal
    deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

  -- | The message type in 'LogEnv'.
  --
  -- @since 0.1.0.0
  type LogMessage = String

  -- | The type of environment for logging.
  --
  -- @since 0.1.0.0
  newtype LogEnv = LogEnv
    { logFunc :: LogLevel -> LogMessage -> IO ()
    } deriving (Typeable)

  -- | Make 'LogEnv' strictly.
  --
  -- @since 0.1.0.0
  sqLogEnv :: (LogLevel -> LogMessage -> IO ()) -> LogEnv
  sqLogEnv f = f `seq` LogEnv f

  -- | Environment values with a logging function.
  --
  -- @since 0.1.0.0
  class HasLogEnv env where
    logEnvL :: Lens' env LogEnv

  -- | @since 0.1.0.0
  instance HasLogEnv LogEnv where
    logEnvL = id

  -- | Log a message with a given level.
  --
  -- @since 0.1.0.0
  logGeneric
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogLevel
    -> LogMessage
    -> m ()
  logGeneric ll lm = do
    env <- ask
    liftIO $ logFunc (view logEnvL env) ll lm

  -- | Log a DEBUG level message.
  --
  -- @since 0.1.0.0
  logDebug
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogMessage
    -> m ()
  logDebug lm = logGeneric LevelDebug lm

  -- | Log a INFO level message.
  --
  -- @since 0.1.0.0
  logInfo
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogMessage
    -> m ()
  logInfo lm = logGeneric LevelInfo lm

  -- | Log a WARN level message.
  --
  -- @since 0.1.0.0
  logWarn
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogMessage
    -> m ()
  logWarn lm = logGeneric LevelWarn lm

  -- | Log a ERROR level message.
  --
  -- @since 0.1.0.0
  logError
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogMessage
    -> m ()
  logError lm = logGeneric LevelError lm

  -- | Log a FATAL level message.
  --
  -- @since 0.1.0.0
  logFatal
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogMessage
    -> m ()
  logFatal lm = logGeneric LevelFatal lm
