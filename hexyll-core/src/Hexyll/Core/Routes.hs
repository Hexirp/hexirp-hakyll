module Hexyll.Core.Routes where

  import Prelude

  import Path

  import Hexyll.Core.Identifier

  newtype Routes = Routes { unRoutes :: Identifier -> [Path Rel File] }
