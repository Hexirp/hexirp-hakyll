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
      f0 (Coroutine x) = Coroutine (fmap f2 x)
      f2 (Left x     ) = Left (fmap f0 x)
      f2 (Right x    ) = Right (f x)

  instance (Functor s, Applicative m) => Applicative (Coroutine s m) where
    pure x = Coroutine (pure (Right x))
    x <*> y = f0 x y where
      -- See https://github.com/ekmett/free/blob/7be30d7cd139a7f40a5e76cd3cb126534c239231/src/Control/Monad/Free.hs#L219-L225 .
      -- That is the implementation of 'Functor f => Applicative (Free f)' in free-5.1.3.
      -- This implementation is based on it.
      f0 :: forall s m a b. (Functor s, Applicative m) => Coroutine s m (a -> b) -> Coroutine s m a -> Coroutine s m b
      f0 (Coroutine x0) (Coroutine y0) = Coroutine (f2 <$> x0 <*> y0)
      f2 :: forall s m a b. (Functor s, Applicative m) => Either (s (Coroutine s m (a -> b))) (a -> b) -> Either (s (Coroutine s m a)) a -> Either (s (Coroutine s m b)) b
      f2 (Left x2     )  y2            = Left (f3 x2 (Coroutine (pure y2)))
      f2 (Right x2    ) (Left y2     ) = Left (fmap (fmap x2) y2)
      f2 (Right x2    ) (Right y2    ) = Right (x2 y2)
      f3 :: forall s m a b. (Functor s, Applicative m) => s (Coroutine s m (a -> b)) -> Coroutine s m a -> s (Coroutine s m b)
      f3  x3             y3            = fmap (\x' -> f0 x' y3) x3

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
