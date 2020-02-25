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

  import Data.Typeable ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO, liftIO )
  import Control.Monad.Reader.Class ( MonadReader ( ask ) )

  import Lens.Micro        ( Lens' )
  import Lens.Micro.Extras ( view )

  import Hexyll.Core.Log ( LogLevel, LogMessage )

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
  logGenericEnv
    :: (MonadIO m, MonadReader env m, HasLogEnv env)
    => LogLevel
    -> LogMessage
    -> m ()
  logGenericEnv ll lm = do
    env <- ask
    liftIO $ let logEnv = view logEnvL env in
      logFunc logEnv (logOption logEnv) ll lm

  data LogOption = LogOption
    { logMinLevel :: LogLevel
    , logSource :: String
    , logIndentLevel :: Int
    } deriving (Eq, Ord, Show, Typeable)
