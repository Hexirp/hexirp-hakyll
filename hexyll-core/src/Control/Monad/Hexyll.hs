-- |
-- Module:      Control.Monad.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes extra functions of "Control.Monad".
module Control.Monad.Hexyll
  ( orM
  ) where

  import Prelude
  import Control.Monad

  -- | A version of 'or' lifted to a monad. Retains the short-circuiting
  -- behaviour.
  --
  -- >>> orM [ Just False, Just False, Just False ]
  -- Just False
  --
  -- >>> orM [ Just False, Just True, undefined ]
  -- Just True
  --
  -- prop> Just (or xs) == orM (map Just xs)
  --
  -- 'orM' comes from @orM@ in @Control.Monad.Extra@ in @extra-1.6.18@.
  --
  -- @since 0.1.0.0
  orM :: [IO Bool] -> IO Bool
  orM []       = return False
  orM (x : xs) = x >>= \b -> if b then return True else orM xs
