module Data.Time.Hexyll where

  import Prelude

  import Data.Typeable ( Typeable )

  import Control.DeepSeq ( NFData (..) )

  import Data.Time

  import Data.Binary ( Binary (..) )

  newtype BinaryTime = BinaryTime { unBinaryTime :: UTCTime }
    deriving ( Eq, Ord, Show, Typeable )

  instance NFData BinaryTime where
    rnf (BinaryTime x) = rnf x

  instance Binary BinaryTime where
    put (BinaryTime t) = case t of
      UTCTime (ModifiedJulianDay d) dt -> do
        put d
        put (toRational dt)
    get = do
      d <- get
      dt' <- get
      return (BinaryTime (UTCTime (ModifiedJulianDay d) (fromRational dt')))
