{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Hexyll.Core.LogEnv
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: non-portable (multi-parameter type classes)
--
-- This module provides an environment for logging.
--
-- @since 0.1.0.0
module Hexyll.Core.LogEnv where

  import Prelude

  import Control.Monad ( when )

  import Data.Typeable  ( Typeable )
  import Data.Semigroup ( stimesMonoid )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import System.IO ( stdout )

  import Data.ByteString.Builder ( hPutBuilder, stringUtf8 )

  import Hexyll.Core.Log

  -- | The type of environment for logging.
  --
  -- @since 0.1.0.0
  data LogEnv = LogEnv
    { logFunc :: !(LogOption -> LogLevel -> LogMessage -> IO ())
    , logOption :: !LogOption
    } deriving Typeable

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
  logGenericE
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogLevel
    -> LogMessage
    -> m ()
  logGenericE ll lm = do
    env <- ask
    liftIO $ let logEnv = view logEnvL env in
      logFunc logEnv (logOption logEnv) ll lm

  -- | The option of 'LogEnv'.
  --
  -- @since 0.1.0.0
  data LogOption = LogOption
    { logMinLevel :: !LogLevel
    , logSource :: !String
    , logIndentLevel :: !Int
    } deriving (Eq, Ord, Show, Typeable)

  -- | A simple logging function.
  --
  -- @since 0.1.0.0
  simpleLogFunc :: LogOption -> LogLevel -> LogMessage -> IO ()
  simpleLogFunc lo ll lm =
      when (logMinLevel lo <= ll) $
        hPutBuilder stdout $ header <> " " <> stringUtf8 lm
    where
      indent = stimesMonoid (logIndentLevel lo) " "
      level = case ll of
        LevelDebug -> "[DEBUG]"
        LevelInfo -> "[INFO]"
        LevelWarn -> "[WARN]"
        LevelError -> "[Error]"
        LevelFatal -> "[FATAL]"
      source = stringUtf8 $ logSource lo
      header = indent <> level <> ":" <> source <> ":"

  -- | A simple 'LogEnv'. This is not intended to be performed in parallel.
  --
  -- @since 0.1.0.0
  simpleLogEnv :: LogEnv
  simpleLogEnv = LogEnv
    { logFunc = simpleLogFunc
    , logOption = LogOption
      { logMinLevel = LevelWarn
      , logSource = ""
      , logIndentLevel = 0
      }
    }
