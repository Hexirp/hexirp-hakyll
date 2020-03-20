-- |
-- Module:      Data.Time.Hexyll
-- Copyright:   (c) 2019 Hexirp
-- License:     Apache-2.0
-- Maintainer:  https://github.com/Hexirp/hexirp-hakyll
-- Stability:   unstable
-- Portability: portable
--
-- This module includes types and functions about "Data.Time".
--
-- @since 0.1.0.0
module Data.Time.Hexyll where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Data.Time

  import Data.Binary ( Binary (..) )

  -- | A wrapper for the instance @'Binary' 'UTCTime'@.
  --
  -- @since 0.1.0.0
  newtype BinaryTime = BinaryTime { unBinaryTime :: UTCTime }
    deriving ( Eq, Ord, Show, Typeable )

  -- | @since 0.1.0.0
  instance NFData BinaryTime where
    rnf (BinaryTime x) = rnf x

  -- | @since 0.1.0.0
  instance Binary BinaryTime where
    put (BinaryTime t) = case t of
      UTCTime (ModifiedJulianDay d) dt -> do
        put d
        put (toRational dt)
    get = do
      d <- get
      dt' <- get
      return (BinaryTime (UTCTime (ModifiedJulianDay d) (fromRational dt')))
