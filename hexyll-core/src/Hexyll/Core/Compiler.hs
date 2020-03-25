module Hexyll.Core.Compiler where

  import Prelude

  import Control.Monad.Trans.RWS.Strict ( RWST (..) )
  import Control.Monad.Coroutine        ( Coroutine (..) )

  import Hexyll.Core.Identifier

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
