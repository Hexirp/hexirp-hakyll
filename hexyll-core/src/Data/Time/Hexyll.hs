module Data.Time.Hexyll where

  import Prelude

  import Data.Time

  import Data.Binary ( Binary (..) )

  newtype BinaryTime = BinaryTime { unBinaryTime :: UTCTime }

  instance Binary BinaryTime where
    put = undefined
    get = undefined
