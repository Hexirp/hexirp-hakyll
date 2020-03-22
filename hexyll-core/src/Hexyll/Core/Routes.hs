{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.Routes
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Routes', a type to specify files to write compilation
-- results.
--
-- @since 0.1.0.0
module Hexyll.Core.Routes where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import           Path
  import qualified Path.Hexyll

  import Hexyll.Core.Identifier
  import Hexyll.Core.Identifier.Pattern

  -- | A type of routing.
  --
  -- If a 'Identifier' given, it returns the output destinations. Returning
  -- an empty list means that there is no output. Returning a list containing
  -- two elements means that there are two files to output.
  --
  -- @since 0.1.0.0
  newtype Routes = Routes { unRoutes :: Identifier -> [Path Rel File] }
    deriving ( Typeable )

  -- | @since 0.1.0.0
  instance NFData Routes where
    rnf (Routes x) = rnf x

  -- | @since 0.1.0.0
  instance Semigroup Routes where
    Routes f <> Routes g = Routes (\i -> f i <> g i)

  -- | @since 0.1.0.0
  instance Monoid Routes where
    mempty = Routes (\_ -> [])

  -- | Output to the same location as the resource.
  --
  -- @since 0.1.0.0
  idRoute :: Routes
  idRoute = Routes $ \i -> [fromIdentifierToPath i]

  -- | Change the extension.
  --
  -- @since 0.1.0.0
  setExtension :: String -> Routes
  setExtension ext = Routes $ \i ->
    case Path.Hexyll.replaceExtension ext $ fromIdentifierToPath i of
      Left e -> error $ unwords
        [ "setExtension: "
        , "Something wrong happened."
        , "'replaceExtension' threw an error:"
        , show (show e)
        ]
      Right p -> [p]

  -- | Apply the 'Routes' only when the conditions are met.
  --
  -- @since 0.1.0.0
  matchRoute :: Pattern -> Routes -> Routes
  matchRoute pat (Routes f) = Routes $ \i ->
    if match i pat then f i else []
