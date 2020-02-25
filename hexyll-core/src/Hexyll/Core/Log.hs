module Hexyll.Core.Log where

  import Prelude

  -- | The log level of a message in 'LogEnv'. This comes from apache log4j.
  --
  -- @since 0.1.0.0
  data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelFatal
    deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

  -- | The message type in 'LogEnv'.
  --
  -- @since 0.1.0.0
  type LogMessage = String

  class Monad m => MonadLog m where
    logGeneric :: LogLevel -> LogMessage -> m ()
