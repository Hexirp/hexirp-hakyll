{-# LANGUAGE ExplicitForAll #-}

module Hexyll.Core.Compiler where

  import Prelude

  import Control.Monad.Trans.RWS.Strict ( RWST (..) )

  import Hexyll.Core.Identifier

  newtype Coroutine s m a = Coroutine
    { unCoroutine :: m (Either (s (Coroutine s m a)) a)
    }

  instance (Functor s, Functor m) => Functor (Coroutine s m) where
    fmap f = f0 where
      f0 (Coroutine x) = Coroutine (f1 x)
      f1  x            = fmap f2 x
      f2 (Left s     ) = Left (f3 s)
      f2 (Right x    ) = Right (f x)
      f3  x            = fmap f0 x

  instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
    pure x = Coroutine (pure (Right x))
    x <*> y = f0 x y where
      f0 :: forall s m a b. (Functor s, Applicative m) => Coroutine s m (a -> b) -> Coroutine s m a -> Coroutine s m b
      f0 (Coroutine x0) (Coroutine y0) = Coroutine (f1 x0 y0)
      f1 :: forall s m a b. (Functor s, Applicative m) => m (Either (s (Coroutine s m (a -> b))) (a -> b)) -> m (Either (s (Coroutine s m a)) a) -> m (Either (s (Coroutine s m b)) b)
      f1  x1             y1            = f2 <$> x1 <*> y1
      f2 :: forall s m a b. (Functor s, Applicative m) => Either (s (Coroutine s m (a -> b))) (a -> b) -> Either (s (Coroutine s m a)) a -> Either (s (Coroutine s m b)) b
      f2 (Left x2     )  y2            = Left (f3 x2 y2)
      f2 (Right x2    )  y2            = undefined
      f3 :: forall s m a b. (Functor s, Applicative m) => s (Coroutine s m (a -> b)) -> Either (s (Coroutine s m a)) a -> s (Coroutine s m b)
      f3  x3             y3            = f4 x3 (pure y3)
      f4 :: forall s m a b. (Functor s, Applicative m) => s (Coroutine s m (a -> b)) -> m (Either (s (Coroutine s m a)) a) -> s (Coroutine s m b)
      f4  x4             y4            = f5 x4 (Coroutine y4)
      f5 :: forall s m a b. (Functor s, Applicative m) => s (Coroutine s m (a -> b)) -> Coroutine s m a -> s (Coroutine s m b)
      f5  x5             y5            = fmap (<*> y5) x5

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
