-- |
-- Module: Data.List.Hexyll
-- Description: Additional functions of Data.List
-- Copyright: (c) 2019 Hexirp
-- License: Apache-2.0
-- Maintainer: https://github.com/Hexirp/hexirp-hakyll
-- Stability: stable
-- Portability: portable
--
-- This module includes additional functions of "Data.List".

module Data.List.Hexyll
  ( breakWhen
  ) where

  import Prelude

  -- | 'breakWhen', like 'break', applied to a predicate @p@ and a list @xs@,
  -- returns a tuple where first element is longest prefix (possibly empty) of
  -- @xs@ of elements whose rest (including itself) do not satisfy @p@ and
  -- second element is the remainder of the list:
  --
  -- >>> breakWhen (\xs -> length xs == 3) [1,2,3,4,1,2,3,4]
  -- ([1,2,3,4,1],[2,3,4])
  --
  -- >>> breakWhen (\xs -> head xs > 3) [1,2,3,4,1,2,3,4]
  -- ([1,2,3],[4,1,2,3,4])
  --
  -- >>> breakWhen (\xs -> head xs > 3) []
  -- ([],[])
  --
  -- >>> breakWhen (const True) [1,2,3,4]
  -- ([],[1,2,3,4])
  --
  -- >>> breakWhen (const False) [1,2,3,4]
  -- ([1,2,3,4],[])
  --
  -- >>> breakWhen null [1,2,3,4]
  -- ([1,2,3,4],[])
  --
  -- @since 0.1.0.0
  breakWhen :: ([a] -> Bool) -> [a] -> ([a], [a])
  breakWhen _ xs@[]      = ([], xs)
  breakWhen p xs@(x:xs')
    | p xs               = ([], xs)
    | otherwise          = case breakWhen p xs' of { (ys, zs) -> (x:ys, zs); }
