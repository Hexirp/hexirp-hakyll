-- |
-- Module:      Hexyll.Core.Dependencies
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides 'outOfDate' that mark resources to be updated as
-- out-of-date.
module Hexyll.Core.Dependencies
  ( Dependency (..)
  , DependencyFacts (..)
  , emptyFacts
  , lookupFacts
  , insertFacts
  , DependencyCache (..)
  , emptyCache
  , lookupCache
  , insertCache
  , IdentifierUniverse
  , IdentifierOutOfDate
  , CalculationLog
  , outOfDate
  ) where

  import Prelude ()

  import Hexyll.Core.Dependencies.Internal
