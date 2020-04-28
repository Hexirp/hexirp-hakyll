--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hexyll.Core.Item.SomeItem
    ( SomeItem (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Binary          (Binary)
import           Data.Typeable        (Typeable)


--------------------------------------------------------------------------------
import           Hexyll.Core.Item        (Item)
import           Hexyll.Core.OldWritable


--------------------------------------------------------------------------------
-- | An existential type, mostly for internal usage.
data SomeItem = forall a.
    (Binary a, Typeable a, Writable a) => SomeItem (Item a)
    deriving (Typeable)
