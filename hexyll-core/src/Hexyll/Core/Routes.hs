module Hexyll.Core.Routes where

  import Prelude

  import Path

  import Hexyll.Core.Identifier

  newtype Routes = Routes { unRoutes :: Identifier -> [Path Rel File] }

  instance Semigroup Routes where
    Routes f <> Routes g = Routes (\i -> f i ++ g i)

  instance Monoid Routes where
    mempty = Routes (\_ -> [])
