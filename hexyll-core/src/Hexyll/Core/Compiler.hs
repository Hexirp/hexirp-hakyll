{-# LANGUAGE ExplicitForAll #-}

module Hexyll.Core.Compiler where

  import Prelude

  import Control.Monad.Trans.Class      ( MonadTrans (..) )
  import Control.Monad.Trans.RWS.Strict ( RWST (..) )

  import Hexyll.Core.Identifier

  newtype Coroutine s m a = Coroutine
    { unCoroutine :: m (Either (s (Coroutine s m a)) a)
    }

  instance (Functor s, Functor m) => Functor (Coroutine s m) where
    fmap f = f0 where
      f0 (Coroutine x) = Coroutine (fmap f2 x)
      f2 (Left x     ) = Left (fmap f0 x)
      f2 (Right x    ) = Right (f x)

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

  instance (Functor s, Monad m) => Monad (Coroutine s m) where
    x >>= y = f0 x y where
      f0 :: forall s m a b. (Functor s, Monad m) => Coroutine s m a -> (a -> Coroutine s m b) -> Coroutine s m b
      f0 x0 y0 = Coroutine (unCoroutine x0 >>= flip f2 y0)
      f2 :: forall s m a b. (Functor s, Monad m) => Either (s (Coroutine s m a)) a -> (a -> Coroutine s m b) -> m (Either (s (Coroutine s m b)) b)
      f2 (Left  x2) y2 = pure (Left (fmap (flip f0 y2) x2))
      f2 (Right x2) y2 = unCoroutine (y2 x2)

  instance Functor s => MonadTrans (Coroutine s) where
    lift x = Coroutine (fmap Right x)

  type Snapshot = String

  data CompilerErrors a = CompilerErrors

  data CompilerSuspend a
    = CompilerSnapshot Snapshot a
    | CompilerRequire Identifier Snapshot a
    | CompilerError (CompilerErrors String)

  data CompilerRead = CompilerRead

  data CompilerWrite = CompilerWrite

  newtype Compiler a = Compiler
    { unCompiler
      :: RWST CompilerRead CompilerWrite () (Coroutine CompilerSuspend IO) a
    }
