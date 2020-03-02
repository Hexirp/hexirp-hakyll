# Hexyll.Core.Log

このモジュールはログをつかさどっている。ログに関する型と、ロギングの作用を持つモナドを表す型クラスを含む。関連しているものとして、具体的な実装であるところの `Hexyll.Core.LogEnv` が存在している。

## 設計案

様々な設計案を出したが、取り敢えずは一つに決めることにした。

### 1

```haskell
type StoreKey = [String]

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: (Binary a, Typeable a) => StoreKey -> a -> m ()
  set :: (Binary a, Typeable a) => StoreKey -> m (StoreResult a)
```

### 2

```haskell
type StoreKey = [String]

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: (Binary a, Typeable a) => StoreKey -> a -> m ()
  set :: (Binary a, Typeable a) => StoreKey -> m (StoreResult a)
```

### 3

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: (Binary a, Typeable a) => StoreKey -> a -> m ()
  set :: (Binary a, Typeable a) => StoreKey -> m (StoreResult a)
```

### 4

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: Typeable a => StoreKey -> a -> m ()
  set :: Typeable a => StoreKey -> m (StoreResult a)
```

### 5

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: Typeable a => StoreKey -> a -> m ()
  set :: Typeable a => StoreKey -> m (StoreResult a)
  remove :: StoreKey -> m ()
```

### 6

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: Typeable a => StoreKey -> a -> m ()
  set :: Typeable a => StoreKey -> m (StoreResult a)
  remove :: StoreKey -> m Bool
```

### 7

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: Typeable a => StoreKey -> a -> m ()
  set :: Typeable a => StoreKey -> m (StoreResult a)
  remove :: Typeable a => StoreKey -> proxy a -> m (StoreResult ())
```

### 8

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  get :: Typeable a => StoreKey -> a -> m ()
  set :: Typeable a => StoreKey -> m (StoreResult a)
  remove :: Typeable a => StoreKey -> proxy a -> m (StoreResult (proxy a))
```

### 9

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  save :: Typeable a => StoreKey -> a -> m ()
  load :: Typeable a => StoreKey -> m (StoreResult a)
  remove :: StoreKey -> m Bool
```

### 10

```haskell
type StoreKey = String

data StoreResult a
  = StoreFound a
  | StoreNotFound
  | StoreWrongType TypeRep
  deriving (Eq, Show, Typeable)

class Monad m => MonadStore m where
  save :: Typeable a => StoreKey -> a -> m ()
  load :: Typeable a => StoreKey -> m (StoreResult a)
  isMember :: StoreKey -> m Bool
  remove :: StoreKey -> m Bool
```

### 11

```haskell
type StoreKey = [String]

data StoreValue where
  StoreValue :: (Binary a, Typeable a) => a -> StoreValue

class Monad m => MonadStore m where
  save :: StoreKey -> StoreValue -> m ()
  load :: StoreKey -> m (Maybe StoreValue)
```

### 12

```haskell
type StoreKey = [String]

data StoreValue where
  StoreValue :: (Binary a, Typeable a) => a -> StoreValue

class Monad m => MonadStore m where
  save :: StoreKey -> StoreValue -> m ()
  loadDelay :: StoreKey -> m (Maybe (m StoreValue))

  load :: StoreKey -> m (Maybe StoreValue)
  load sk = do
    mmsv <- loadDelay sk
    case mmsv of
      Nothing -> return Nothing
      Just msv -> msv
```

### 13

```haskell
type StoreKey = [String]

data StoreValue where
  StoreValue :: (Binary a, Typeable a) => a -> StoreValue

class Monad m => MonadStore m where
  save :: StoreKey -> StoreValue -> m ()
  loadDelay :: StoreKey -> m (Maybe (m StoreValue))

  load :: StoreKey -> m (Maybe StoreValue)
  load sk = do
    mmsv <- loadDelay sk
    case mmsv of
      Nothing -> return Nothing
      Just msv -> msv

  isMember :: StoreKey -> m Bool
  isMember sk = do
    mmsv <- loadDelay sk
    return $ isJust mmsv
```

### 14

```haskell
  type StoreKey = String

  newtype StoreResult m a = StoreResult
    { unStoreResult :: Maybe (m (Either StoreError a))
    } deriving ( Typeable )

  data StoreError = StoreError
    { storeExpect :: TypeRep
    , storeActual :: TypeRep
    } deriving ( Eq, Show, Typeable )

  class Monad m => MonadStore m where
    save :: (Binary a, Typeable a) => StoreKey -> a -> m ()
    loadDelay :: (Binary a, Typeable a) => StoreKey -> m (StoreResult m a)

  load
    :: (Binary a, Typeable a, MonadStore m)
    => StoreKey
    -> m (Maybe (Either StoreError a))
  load sk = do
    StoreResult mmesv <- loadDelay sk
    case mmesv of
      Nothing -> return Nothing
      Just mesv -> Just <$> mesv

  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    StoreResult mmesv <- loadDelay @_ @() sk
    return $ isJust mmesv
```

### 15

```haskell
  -- | A key for 'MonadStore'.
  --
  -- @since 0.1.0.0
  type StoreKey = String

  -- | A value for 'MonadStore'.
  --
  -- @since 0.1.0.0
  data StoreValue where
    MkStoreValue :: (Binary a, Typeable a) => !a -> StoreValue

  -- | A type of delayed loading.
  --
  -- @since 0.1.0.0
  newtype StoreLoad m = StoreLoad
    { runStoreLoad
        :: forall a. (Binary a, Typeable a) => m (Either StoreError a)
    } deriving ( Typeable )

  -- | An error type when casting.
  --
  -- @since 0.1.0.0
  data StoreError = StoreError
    { storeExpected :: TypeRep
    , storeActual :: TypeRep
    } deriving ( Eq, Ord, Show, Typeable )

  -- | A monad for handling a store. This has two functions corresponding to
  -- save/load.
  --
  -- 'loadDelay' delays loading values. Checking for the existence of a value
  -- and loading it have different costs.
  --
  -- @since 0.1.0.0
  class Monad m => MonadStore m where
    -- | Save a value with a key.
    save :: StoreKey -> StoreValue -> m ()
    -- | Load a value lazily.
    loadDelay :: StoreKey -> m (Maybe (StoreLoad m))

  -- | Load a value. The result branches in two points. The points is whether
  -- a value exists and if the type match.
  --
  -- @since 0.1.0.0
  load
    :: (MonadStore m, Binary a, Typeable a)
    => StoreKey
    -> m (Maybe (Either StoreError a))
  load sk = do
    msl <- loadDelay sk
    case msl of
      Nothing -> pure Nothing
      Just sl -> Just <$> runStoreLoad sl

  -- | Check if a value exists.
  --
  -- @since 0.1.0.0
  isExistent :: MonadStore m => StoreKey -> m Bool
  isExistent sk = do
    msl <- loadDelay sk
    return $ isJust msl
```
