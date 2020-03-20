module Data.Time.Hexyll where

  import Prelude

  import Data.Time

  import Data.Binary ( Binary (..) )

  newtype BinaryTime = BinaryTime { unBinaryTime :: UTCTime }

  instance Binary BinaryTime where
    put (BinaryTime t) = case t of
      UTCTime (ModifiedJulianDay d) dt -> do
        put d
        put (toRational dt)
    get = do
      d <- get
      dt' <- get
      return (BinaryTime (UTCTime (ModifiedJulianDay d) (fromRational dt')))
