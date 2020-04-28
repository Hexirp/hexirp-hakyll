{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Lens.Micro.Hexyll
-- Copyright:   (c) 2020 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes additional functions of "Lens.Micro" and
-- "Lens.Micro.Hexyll".
--
-- @since 0.1.0.0
module Lens.Micro.Hexyll where

  import Prelude

  import Control.Monad.Reader.Class (MonadReader (ask))

  import Lens.Micro        (Getting)
  import Lens.Micro.Extras (view)

  -- | Ask the environment and apply a getter to it.
  --
  -- I know there is a function @view@ in @Lens.Micro.Mtl@ in <http://hackage.haskell.org/package/microlens-mtl microlens-mtl>.
  -- But I want a better named function.
  --
  -- @since 0.1.0.0
  askView :: MonadReader env m => Getting a env a -> m a
  askView l = do
    env <- ask
    return $ view l env
