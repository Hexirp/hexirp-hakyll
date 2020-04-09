module Hexyll.Core.Item where

  import Prelude

  import Data.Typeable ( Typeable )

  import Data.Binary ( Binary (..) )

  import Hexyll.Core.Identifier

  -- | An item is a combination of some content and its 'Identifier'.
  --
  -- It's other type represents compilation results.
  --
  -- @since 0.1.0.0
  data Item a = Item
    { itemIdentifier :: !Identifier
    , itemBody       :: !a
    } deriving (Eq, Ord, Show, Typeable)

  -- | @since 0.1.0.0
  instance Functor Item where
    fmap f (Item i x) = Item i (f x)

  -- | @since 0.1.0.0
  instance Foldable Item where
    foldr f z (Item _ x) = f x z

  -- | @since 0.1.0.0
  instance Traversable Item where
    traverse f (Item i x) = Item i <$> f x

  -- | @since 0.1.0.0
  instance Binary a => Binary (Item a) where
    put (Item i x) = do
      put i
      put x
    get = do
      i <- get
      x <- get
      return $ Item i x

  -- | Set an item body.
  --
  -- @since 0.1.0.0
  setItemBody :: a -> Item b -> Item a
  setItemBody x (Item i _) = Item i x
