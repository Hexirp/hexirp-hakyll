-- |
-- Module:      Hexyll.Core.Identifier.Pattern
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module defines a type of pattern matching to 'Identifier'.
--
-- 'PrimPattern' is a primitive pattern. 'PatternExpr' is an expression of
-- pattern. 'PatternConj' is a conjunction of 'PatternExpr's. 'PatternDisj' is
-- a disjunction of 'PatternExpr's. 'Pattern' is a pattern.
--
-- I recommend to use 'PatternExpr'. If you want to the instance of 'Monoid',
-- you can use 'PatternConj' or 'PatternDisj'. I was able to define the
-- instance @'Monoid' 'PatternExpr'@ with @('.&&.')@ or @('.||.')@. But, it is
-- not a real monoid because of @x .&&. (y .&&. z) /= (x .&&. y) .&&. z@ and
-- @x .||. (y .||. z) /= (x .||. y) .||. z@.
module Hexyll.Core.Identifier.Pattern
  ( PatternExpr
  , fromGlob
  , fromRegex
  , fromVersion
  , everything
  , (.&&.)
  , nothing
  , (.||.)
  , complement
  , toPatternConj
  , toPatternDisj
  , matchExpr
  , PatternConj ( PatternConj )
  , unPatternConj
  , matchConj
  , PatternDisj ( PatternDisj )
  , unPatternDisj
  , matchDisj
  , Pattern ( Pattern )
  , runPattern
  , compileExpr
  , compileConj
  , compileDisj
  , match
  ) where

  import Prelude

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern.Internal
