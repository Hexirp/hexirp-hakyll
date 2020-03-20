{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module:      Hexyll.Core.Resource
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   stable
-- Portability: portable
--
-- This module defines 'Resource', a type used to uniquely identify a resource.
--
-- @since 0.1.0.0
module Hexyll.Core.Resource where

  import Prelude

  import Data.Typeable   ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Data.Binary ( Binary (..) )

  import Path

  -- | A path of a resource.
  --
  -- This type is used to uniquely identify a resource. It is a file path
  -- under the provider directory.
  --
  -- @since 0.1.0.0
  data Resource = Resource { unResource :: Path Rel File }
    deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance NFData Resource where
    rnf (Resource x) = rnf x

  -- | @since 0.1.0.0
  instance Binary Resource where
    put (Resource x) = put (toFilePath x)
    get = do
      x' <- get
      case parseRelFile x' of
        Nothing -> error "Data.Binary.get: Invalid Resource"
        Just x -> return (Resource x)
