module Hexyll.Core.Item where

  import Prelude

  import Data.Typeable ( Typeable )

  import Data.Binary ( Binary (..) )

  import Hexyll.Core.Identifier

  -- | An item is a combination of some content and its 'Identifier'.
  data Item a = Item
    { itemIdentifier :: !Identifier
    , itemBody       :: !a
    } deriving (Eq, Ord, Show, Typeable)

  instance Functor Item where
    fmap f (Item i x) = Item i (f x)

  instance Foldable Item where
    foldr f z (Item _ x) = f x z

  instance Traversable Item where
    traverse f (Item i x) = Item i <$> f x

  instance Binary a => Binary (Item a) where
    put (Item i x) = do
      put i
      put x
    get = do
      i <- get
      x <- get
      return $ Item i x

  itemSetBody :: a -> Item b -> Item a
  itemSetBody x (Item i _) = Item i x
