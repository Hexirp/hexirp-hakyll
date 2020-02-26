-- |
-- Module:      Hexyll.Core.Log
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides the basic types and type classes for logging.
--
-- @since 0.1.0.0
module Hexyll.Core.Log where

  import Prelude

  import Data.Typeable ( Typeable )

  -- | The log level of a message in the log. This comes from apache log4j.
  --
  -- @since 0.1.0.0
  data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelFatal
    deriving (Eq, Ord, Enum, Bounded, Show, Typeable)

  -- | The message type in the log.
  --
  -- @since 0.1.0.0
  type LogMessage = String

  -- | A monad for logging. This has a simple function 'logGeneric' which
  -- requires 'LogLevel' and 'LogMessage' and put a log.
  class Monad m => MonadLog m where
    logGeneric :: LogLevel -> LogMessage -> m ()

  -- | Log a DEBUG level message.
  --
  -- @since 0.1.0.0
  logDebug :: MonadLog m => LogMessage -> m ()
  logDebug lm = logGeneric LevelDebug lm

  -- | Log a INFO level message.
  --
  -- @since 0.1.0.0
  logInfo :: MonadLog m => LogMessage -> m ()
  logInfo lm = logGeneric LevelInfo lm

  -- | Log a WARN level message.
  --
  -- @since 0.1.0.0
  logWarn :: MonadLog m => LogMessage -> m ()
  logWarn lm = logGeneric LevelWarn lm

  -- | Log a ERROR level message.
  --
  -- @since 0.1.0.0
  logError :: MonadLog m => LogMessage -> m ()
  logError lm = logGeneric LevelError lm

  -- | Log a FATAL level message.
  --
  -- @since 0.1.0.0
  logFatal :: MonadLog m => LogMessage -> m ()
  logFatal lm = logGeneric LevelFatal lm
