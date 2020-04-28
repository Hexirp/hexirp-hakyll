{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module:      Hexyll.Core.Compiler.Internal
-- Copyright:   (c) 2020 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   internal
-- Portability: non-portable (multi-parameter type classes)
--
-- This is an internal module for "Hexyll.Core.Compiler".
--
-- @since 0.1.0.0
module Hexyll.Core.Compiler.Internal where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.Monad.IO.Class     ( MonadIO (..) )
  import Control.Monad.Trans.Class  ( MonadTrans (..) )
  import Control.Monad.Trans.Reader ( ReaderT (..) )
  import Control.Monad.Reader.Class ( MonadReader (..) )

  import Lens.Micro        ( lens )

  import qualified Data.Set as S

  import Data.IORef ( IORef )

  import Hexyll.Core.Configuration
  import Hexyll.Core.Identifier
  import Hexyll.Core.Routes
  import Hexyll.Core.Dependencies
  import Hexyll.Core.Log
  import Hexyll.Core.LogEnv
  import Hexyll.Core.Store
  import Hexyll.Core.StoreEnv
  import Hexyll.Core.Provider
  import Hexyll.Core.ProviderEnv
  import Hexyll.Core.Universe
  import Hexyll.Core.UniverseEnv

  -- | Coroutine monad. This monad can be suspended and resumed.
  --
  -- @since 0.1.0.0
  newtype Coroutine s m a = Coroutine
    { unCoroutine :: m (Either (s (Coroutine s m a)) a)
    }

  -- | @since 0.1.0.0
  instance (Functor s, Functor m) => Functor (Coroutine s m) where
    fmap f = f0 where
      f0 (Coroutine x) = Coroutine (fmap f2 x)
      f2 (Left x     ) = Left (fmap f0 x)
      f2 (Right x    ) = Right (f x)

  -- | @since 0.1.0.0
  instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
    pure x = Coroutine (pure (Right x))
    x <*> y = f0 x y where
      -- See https://github.com/ekmett/free/blob/7be30d7cd139a7f40a5e76cd3cb126534c239231/src/Control/Monad/Free.hs#L219-L225 .
      -- That is the implementation of 'Functor f => Applicative (Free f)' in free-5.1.3. This implementation is based on it.
      f0 :: forall s m a b. (Functor s, Applicative m) => Coroutine s m (a -> b) -> Coroutine s m a -> Coroutine s m b
      f0 x0 y0 = Coroutine (f2 <$> unCoroutine x0 <*> unCoroutine y0)
      f2 :: forall s m a b. (Functor s, Applicative m) => Either (s (Coroutine s m (a -> b))) (a -> b) -> Either (s (Coroutine s m a)) a -> Either (s (Coroutine s m b)) b
      f2 (Left  x2)  y2        = Left (fmap (flip f0 (Coroutine (pure y2))) x2)
      f2 (Right x2) (Left  y2) = Left (fmap (fmap x2) y2)
      f2 (Right x2) (Right y2) = Right (x2 y2)

  -- | @since 0.1.0.0
  instance (Functor s, Monad m) => Monad (Coroutine s m) where
    x >>= y = f0 x y where
      f0 :: forall s m a b. (Functor s, Monad m) => Coroutine s m a -> (a -> Coroutine s m b) -> Coroutine s m b
      f0 x0 y0 = Coroutine (unCoroutine x0 >>= flip f2 y0)
      f2 :: forall s m a b. (Functor s, Monad m) => Either (s (Coroutine s m a)) a -> (a -> Coroutine s m b) -> m (Either (s (Coroutine s m b)) b)
      f2 (Left  x2) y2 = pure (Left (fmap (flip f0 y2) x2))
      f2 (Right x2) y2 = unCoroutine (y2 x2)

  -- | @since 0.1.0.0
  instance Functor s => MonadTrans (Coroutine s) where
    lift x = Coroutine (fmap Right x)

  -- | @since 0.1.0.0
  instance (Functor s, MonadIO m) => MonadIO (Coroutine s m) where
    liftIO x = Coroutine (fmap Right (liftIO x))

  -- | 'Phase' is compilation phases.
  --
  -- A compiler has a set of phases corresponding to it. Any compiler can send
  -- a message during execution that a phase has passed.
  --
  -- @since 0.1.0.0
  newtype Phase = Phase { unPhase :: String }
    deriving (Eq, Ord, Show, Typeable)

  -- | 'PassageMarker' is passage markers.
  --
  -- 'PassageMarker' guarantees that a 'Compiler' has passed through a 'Phase'.
  --
  -- @since 0.1.0.0
  data PassageMarker = PassageMarker
    { pasMrkIdent :: Identifier
    , pasMrkPhase :: Phase
    } deriving (Eq, Ord, Show, Typeable)

  -- | 'CompilerSuspend' is the effect of 'Compiler'.
  --
  -- 'CompilerPassage' is to send a message that passed the phase.
  -- 'CompilerRequire' is to wait until the desired message is sent.
  --
  -- @since 0.1.0.0
  data CompilerSuspend a
    = CompilerPassage Phase a
    | CompilerRequire PassageMarker a

  -- | @since 0.1.0.0
  instance Functor CompilerSuspend where
    fmap f (CompilerPassage ph x) = CompilerPassage ph (f x)
    fmap f (CompilerRequire pm x) = CompilerRequire pm (f x)

  -- | @since 0.1.0.0
  data CompilerRead = CompilerRead
    { compilerConfig :: !Configuration
    , compilerUnderlying :: !Identifier
    , compilerProviderEnv :: !ProviderEnv
    , compilerUniverseEnv :: !UniverseEnv
    , compilerRoutes :: !Routes
    , compilerLogEnv :: !LogEnv
    , compilerWrite :: !(IORef CompilerWrite)
    }

  -- | @since 0.1.0.0
  instance HasConfiguration CompilerRead where
    configurationL =
      lens compilerConfig (\env config -> env { compilerConfig = config })

  -- | @since 0.1.0.0
  instance HasProviderEnv CompilerRead where
    providerEnvL =
      lens compilerProviderEnv (\env prv -> env { compilerProviderEnv = prv })

  -- | @since 0.1.0.0
  instance HasStoreEnv CompilerRead where
    storeEnvL = providerEnvL . storeEnvL

  -- | @since 0.1.0.0
  instance HasUniverseEnv CompilerRead where
    universeEnvL =
      lens compilerUniverseEnv (\env uni -> env { compilerUniverseEnv = uni })

  -- | @since 0.1.0.0
  instance HasLogEnv CompilerRead where
    logEnvL =
      lens compilerLogEnv (\env lge -> env { compilerLogEnv = lge })

  -- | 'CompilerWrite' is the information that the compiler must keep.
  --
  -- @since 0.1.0.0
  data CompilerWrite = CompilerWrite
    { compilerDependencies :: !(S.Set Dependency)
    , compilerCache :: !(S.Set Identifier)
    }

  -- | @since 0.1.0.0
  instance Semigroup CompilerWrite where
    CompilerWrite d0 c0 <> CompilerWrite d1 c1 =
      CompilerWrite (d0 <> d1) (c0 <> c1)

  -- | @since 0.1.0.0
  instance Monoid CompilerWrite where
    mempty = CompilerWrite mempty mempty

  -- | The 'Compiler' type.
  --
  -- @since 0.1.0.0
  newtype Compiler a = Compiler
    { unCompiler
      :: ReaderT CompilerRead (Coroutine CompilerSuspend IO) a
    }

  -- | @since 0.1.0.0
  instance Functor Compiler where
    fmap f (Compiler x) = Compiler (fmap f x)

  -- | @since 0.1.0.0
  instance Applicative Compiler where
    pure x = Compiler (pure x)
    x <*> y = Compiler (unCompiler x <*> unCompiler y)

  -- | @since 0.1.0.0
  instance Monad Compiler where
    x >>= y = Compiler (unCompiler x >>= \x' -> unCompiler (y x'))

  -- | @since 0.1.0.0
  instance MonadIO Compiler where
    liftIO x = Compiler (liftIO x)

  -- | @since 0.1.0.0
  instance MonadReader CompilerRead Compiler where
    ask = Compiler ask
    local f (Compiler x) = Compiler (local f x)

  -- | @since 0.1.0.0
  instance MonadLog Compiler where
    logGeneric = logGenericE

  -- | @since 0.1.0.0
  instance MonadStore Compiler where
    save = saveE
    loadDelay = loadDelayE

  -- | @since 0.1.0.0
  instance MonadProvider Compiler where
    getAllPath = getAllPathE
    getMTimeDelay = getMTimeDelayE
    getBodyDelay = getBodyDelayE

  -- | @since 0.1.0.0
  instance MonadUniverse Compiler where
    getMatches = getMatchesE
    getAllIdentifier = getAllIdentifierE
    countUniverse = countUniverseE
