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
  ( -- * PatternExpr
    PatternExpr
  , -- ** Primitive Combinator
    fromGlob
  , fromRegex
  , fromVersion
  , -- ** Logical Combinator
    everything
  , (.&&.)
  , nothing
  , (.||.)
  , complement
  , -- ** Advanced Combinator
    fromIdentifier
  , fromList
  , fromConj
  , fromDisj
  , -- ** Matching
    matchExpr
  , -- * PatternConj
    PatternConj ( PatternConj )
  , unPatternConj
  , fromExprToConj
  , matchConj
  , -- * PatternDisj
    PatternDisj ( PatternDisj )
  , unPatternDisj
  , fromExprToDisj
  , matchDisj
  , -- * Pattern
    Pattern ( Pattern )
  , runPattern
  , compileExpr
  , compileConj
  , compileDisj
  , match
  ) where

  import Prelude

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern.Internal

  -- | Make a pattern from a 'Identifier'.
  --
  -- The pattern is interpreted as: @'fromIdentifier' i == ('fromGlob'
  -- ('toFilePath' i) '.&&.' 'fromVersion' ('getIdentVersion' i))@.
  --
  -- @since 0.1.0.0
  fromIdentifier :: Identifier -> PatternExpr
  fromIdentifier i =
    fromGlob (toFilePath i) .&&. fromVersion (getIdentVersion i)

  -- | Make a pattern from a list.
  --
  -- The pattern is interpreted as: @'fromList' x == foldr ('.||.') 'nothing'
  -- (map 'fromIdentifier' x)@.
  --
  -- @since 0.1.0.0
  fromList :: [Identifier] -> PatternExpr
  fromList = foldr (\x p -> fromIdentifier x .||. p) nothing

  -- | Make a pattern from a 'PatternConj'.
  --
  -- @since 0.1.0.0
  fromConj :: PatternConj -> PatternExpr
  fromConj (PatternConj p) = foldr (.&&.) nothing p

  -- | Make a pattern from a 'PatternDisj'.
  --
  -- @since 0.1.0.0
  fromDisj :: PatternDisj -> PatternExpr
  fromDisj (PatternDisj p) = foldr (.||.) everything p
