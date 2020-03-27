-- |
-- Module:      Hexyll.Core.Universe
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module provides a type class 'MonadUniverse' for handling the
-- 'Identifier' set.
--
-- @since 0.1.0.0
module Hexyll.Core.Universe where

  import Data.Typeable ( Typeable )

  import Data.String ( IsString (..) )

  import qualified Data.Set as S

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern hiding ( Pattern (..), match )

  -- | A type of patterns for 'MonadUniverse'.
  --
  -- @since 0.1.0.0
  newtype Pattern = Pattern
    { unPattern :: PatternExpr
    } deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance IsString Pattern where
    fromString s = Pattern $ fromString s

  -- | Match a 'Identifier' with a 'Pattern'.
  --
  -- @since 0.1.0.0
  match :: Identifier -> Pattern -> Bool
  match i (Pattern p) = i `matchExpr` p

  -- | Monads that has a set of identifiers and can search for them.
  --
  -- @since 0.1.0.0
  class Monad m => MonadUniverse m where

    -- | Get identifiers that matches the pattern.
    --
    -- @since 0.1.0.0
    getMatches :: Pattern -> m (S.Set Identifier)

    -- | Get all identifiers.
    --
    -- @since 0.1.0.0
    getAllIdentifier :: m (S.Set Identifier)
    getAllIdentifier = getMatches (Pattern everything)

    -- | Count the number of all identifiers.
    --
    -- @since 0.1.0.0
    countUniverse :: m Int
    countUniverse = S.size <$> getMatches (Pattern everything)
