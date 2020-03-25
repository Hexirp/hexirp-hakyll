module Hexyll.Core.Compiler where

  import Prelude

  import Control.Monad.Trans.RWS.Strict ( RWST (..) )

  import Hexyll.Core.Identifier

  newtype Coroutine s m a = Coroutine
    { unCoroutine :: m (Either (s (Coroutine s m a)) a)
    }

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
