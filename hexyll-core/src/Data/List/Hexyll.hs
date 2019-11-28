module Data.List.Hexyll
  ( breakWhen
  ) where

  import Prelude
  import Data.List

  -- | Like 'break', but can act on the entire tail of the list.
  breakWhen :: ([a] -> Bool) -> [a] -> ([a], [a])
  breakWhen _ xs@[]      = ([], xs)
  breakWhen p xs@(x:xs')
    | p xs               = ([], xs)
    | otherwise          = case breakWhen p xs' of { (ys, zs) -> (x:ys, zs); }
