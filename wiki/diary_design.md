# diary\_design

設計について。

## 2020-02-20

設計の日記のようなもの。

### coding

今 (2020-02-20) で分かっている範囲でまとめてみる。

```
    Domain Model

Control.Monad.Hexyll
Data.List.Hexyll
Data.Yaml.Hexyll
Hexyll.Core.Configuration
Hexyll.Core.Identifier.Internal
Hexyll.Core.Identifier
Hexyll.Core.Identifier.Pattern.Glob
Hexyll.Core.Identifier.Pattern.Internal
Hexyll.Core.Identifier.Pattern
Hexyll.Core.Identifier.OldPattern
Hexyll.Core.Dependencies.Internal
Hexyll.Core.Dependencies

    Domain Model (Classes for Infrasturecture)

Hexyll.Core.Metadata

    Infrastructure

Hexyll.Core.Logger
Hexyll.Core.Store
```

こんな感じだろうか。

### modeling

コンパイラーとして。

* ユーザーが、
  * ファイル群をコンパイルする。
    * コンパイルの結果はキャッシュとして保存される。
  * コンパイルの結果をクリアする。
    * キャッシュも同時に削除される。
  * 静的解析を行う。
  * デプロイを行う。

コンパイラー作成のためのライブラリーとして。

* ユーザーが、
  * ルールを実行する。
    * コンフィグを渡せる。
  * ルールを組み立てる。
    * パターンで対象となるリソースをフィルタできる。
    * どの位置に配置されるか指定できる。
    * コンパイラを指定できる。
  * コンパイラを組み立てる。
    * 元となるコンパイラをライブラリから引ける。
    * 識別子で指定してテンプレートを読み込める。
    * アイテムにテンプレートを適用できる。
    * コンパイラからアイテムを得られる。
    * 識別子で指定してリソースを読み込める。
    * コンパイラに引数を渡せる。
    * pandoc を使ったコンパイラを使える。
    * テンプレートを適用するだけの特殊なコンパイラを使える。
    * リソースを取得するだけの特殊なコンパイラを使える。
    * CSSを圧縮できるだけの特殊なコンパイラを使える。
  * コンテクストを組み立てられる。
    * それぞれのコンテクストについて情報の得方を指定できる。
  * フィードページを作ることができる。

これらを整理する。

コンパイラーとして。

* ユーザーが、
  * **ファイル群をコンパイルする。**
    * **コンパイルの結果はキャッシュとして保存される。**
  * **コンパイルの結果をクリアする。**
  * **コンパイルの結果とキャッシュをクリアする。**
  * 静的解析を行う（別のライブラリで特殊なコンパイラとして提供する）。
  * <del>デプロイを行う。</del>

コンパイラーを作成するためのライブラリとして。

* ユーザーが、
  * **ルールを実行する。**
    * **コンフィグを渡せる。**
  * **ルールを組み立てる。**
    * **パターンで対象となるリソースをフィルタできる。**
    * **どの位置に配置されるか（ルート）を指定できる。**
    * **コンパイラを指定できる。**
  * **コンパイラを組み立てる。**
    * **元となるコンパイラをライブラリから引ける。**
    * 識別子で指定してテンプレートを読み込める（テンプレートはリソースの一つであるため統合）。
    * アイテムにテンプレートを適用できる（別のライブラリで提供）。
    * コンパイラからアイテムを得られる（？）。
    * **識別子で指定してリソースを読み込める。**
    * **コンパイラに引数を渡せる。**
    * pandoc を使ったコンパイラを使える（別のライブラリで提供）。
    * テンプレートを適用するだけの特殊なコンパイラを使える（別のライブラリで提供）。
    * **リソースを取得するだけの特殊なコンパイラを使える。**
    * CSSを圧縮できるだけの特殊なコンパイラを使える（別のライブラリで提供）。
  * コンテクストを組み立てられる（？）。
    * それぞれのコンテクストについて情報の得方を指定できる（？）。
  * フィードページを作ることができる（別のライブラリで提供）。
  * **新しいサブコマンドを作ることができる。**

こうなった。まだよくわかっていないところがある。

### coding

`Item` と `Context` について理解したい。

識別子、つまり `Identifier` 型は重要である。このライブラリにおけるリソースは全てが `Identifier` により識別されるのだから。

`Hexyll.Core.Logger` と `Hexyll.Core.Store` は、それぞれログとリソースを集約している。これらは依存関係の逆転を行わなければならないだろう。そして、それは ReaderT デザインパターンにより達成できる。ここで IO に限定されるため純粋なモックが作れなさそうに見えるが、一つの可変参照を認めるだけで近いものを作ることができる。

`Hexyll.Core.Metadata` の `MonadMetadata` も集約に関するものである。その実装の一つは store を使って与えられている。

### modeling

追加で。

* ユーザーが、
  * **コンパイラを組み立てる。**
    * **リソースに付随するメタデータを読み込める。**

## 2020-02-23

ますます設計が必要そうな感じになってきた。

`Item` は `Identifier` とそれが表すアイテムの組である。単純な型である。

`Provider` はコンパイル対象のファイル群を表す。リソースについての追加情報のマップ、ストア、ディレクトリへのファイルパスを含む。リソースについての追加情報は更新時間と対応するメタデータへの識別子である。ここはストアを直接参照しているけど、それは抽象化したほうがいいと思われる。

`Store` はリソースを格納するところである。

`Compiler` は Mealy Machine に類似している。必要な情報を読み込み、 IO モナドの文脈の下で計算し、最終結果として「コンパイルの終了」「スナップショットの出力、続行」「必要なリソースのコンパイルの要求、スナップショットの出力、続行」「エラーの出力、終了」という結果を出す。前は Compiler は Arrow だったようだ。 https://github.com/jaspervdj/hakyll/commit/760b4344377c81922ce5ab4ba05a41f88f45165d で変更されている。ほとんど意味はなかったようだ。スナップショットの意味がわからない。

`Hexyll.Core.Runtime` を読んでみたが、よく分からない。あるリソースのコンパイルの要求があったとき、そのスナップショットがあれば、そのリソースの計算は終了している扱いになるみたい？

```haskell
    Domain

hexyll-core  > [ 1 of 35] Compiling Control.Monad.Hexyll
hexyll-core  > [ 2 of 35] Compiling Data.List.Hexyll
hexyll-core  > [ 3 of 35] Compiling Data.Yaml.Hexyll
hexyll-core  > [ 4 of 35] Compiling Hexyll.Core.Configuration
hexyll-core  > [ 5 of 35] Compiling Hexyll.Core.Identifier.Internal
hexyll-core  > [ 6 of 35] Compiling Hexyll.Core.Identifier
hexyll-core  > [ 7 of 35] Compiling Hexyll.Core.Identifier.Pattern.Glob
hexyll-core  > [ 8 of 35] Compiling Hexyll.Core.Identifier.Pattern.Internal
hexyll-core  > [ 9 of 35] Compiling Hexyll.Core.Identifier.Pattern
hexyll-core  > [15 of 35] Compiling Hexyll.Core.Util.File
hexyll-core  > [20 of 35] Compiling Hexyll.Core.Util.Parser
hexyll-core  > [21 of 35] Compiling Hexyll.Core.Util.String
hexyll-core  > [24 of 35] Compiling Hexyll.Core.Item
hexyll-core  > [27 of 35] Compiling Hexyll.Core.UnixFilter
hexyll-core  > [28 of 35] Compiling Hexyll.Core.Writable
hexyll-core  > [29 of 35] Compiling Hexyll.Core.Item.SomeItem
hexyll-core  > [33 of 35] Compiling Hexyll.Core.File
hexyll-core  > [34 of 35] Compiling Paths_hexyll_core
hexyll-core  > [35 of 35] Compiling System.Directory.Hexyll

hexyll-core  > [13 of 35] Compiling Hexyll.Core.Metadata
hexyll-core  > [22 of 35] Compiling Hexyll.Core.Routes
hexyll-core  > [23 of 35] Compiling Hexyll.Core.Compiler.Internal
hexyll-core  > [25 of 35] Compiling Hexyll.Core.Compiler.Require
hexyll-core  > [26 of 35] Compiling Hexyll.Core.Compiler
hexyll-core  > [30 of 35] Compiling Hexyll.Core.Rules.Internal
hexyll-core  > [32 of 35] Compiling Hexyll.Core.Rules

    Application

hexyll-core  > [10 of 35] Compiling Hexyll.Core.Dependencies.Internal
hexyll-core  > [11 of 35] Compiling Hexyll.Core.Dependencies
hexyll-core  > [31 of 35] Compiling Hexyll.Core.Runtime

    Presentation

    Infrastructure

hexyll-core  > [12 of 35] Compiling Hexyll.Core.Logger
hexyll-core  > [14 of 35] Compiling Hexyll.Core.Store
hexyll-core  > [16 of 35] Compiling Hexyll.Core.Provider.Internal
hexyll-core  > [17 of 35] Compiling Hexyll.Core.Provider.Metadata
hexyll-core  > [18 of 35] Compiling Hexyll.Core.Provider.MetadataCache
hexyll-core  > [19 of 35] Compiling Hexyll.Core.Provider
```

ただのイメージだけで分類した。

### modeling

ロギングについて。

* ユーザーが、
  * ログを追加する。
  * ログにレベルを設定して追加する。
  * ログにインデントを入れる。
  * ログに色を入れる。
  * ログにスレッドの階層構造を反映させる。

## 2020-02-24

Haskell 方面へ寄っていく。

### coding

三つの層を置く。

* ReaderT パターン
* mtl パターン
* 関数

これらが適用できるであろう箇所はいくつかある。

* ログ
* メタデータ
  * 特に、これは `Hexyll.Core.Metadata` の `MonadMetadata` と `Hexyll.Core.Provider` の `Provider` と道具が揃っている。
* ストア

## 2020-02-25

モナドの設計についての考察。

### coding

https://github.com/Hexirp/hexirp-hakyll/blob/2d20aefa0e928db0cd4ba93526a73712949676c5/hexyll-core/src/Hexyll/Core/Compiler/Internal.hs#L75-L196 を見よ。

```haskell
newtype Compiler a = Compiler { unCompiler :: CompilerRead -> IO (CompilerResult a) }

data CompilerResult a = CompilerDone a CompilerWrite | CompilerSnapshot Snapshot (Compiler a) | CompilerRequire (Identifier, Snapshot) (Compiler a) | CompilerError (CompilerErrors String)
```

なぜ、これが仮にでもモナドとして扱えるのか。もちろん、当のモナドの実装は具体的なコードが入り込んでおり、厳密にはモナドとしては扱えない。しかし、それを除いたら実装は正しいように見える。こんな見たことがない形をした型がモナドになれるのか。

色々考えた結論からすると、 Compiler は Coroutine 系モナドである。

```haskell
data CompilerSuspend a = CompilerSnapshot Snapshot a | CompilerRequire Identifier Snapshot a | CompilerError (CompilerErrors String)

type Compiler = ReaderT CompilerRead (WriterT CompilerWrite (Coroutine CompilerSuspend IO))
```

この Coroutine モナドは ReaderT デザインパターンで代替できないモナドの一つである（他には継続モナドなどがある）。コマンドのような命令を表す型を定義する方法で代替できないか考える。

## 2020-02-25

ReaderT デザインパターンを本格的に導入し始めた。 Three Layer Haskell Cake を参考にしている。 Log は導入が終わった。 Store はそもそものモデリングがおかしい。

### coding

Store は二つのキャッシュ先を持っている。メモリ内部とファイルである。これらが互いに補う形となる。

### modeling

Store は何ができるのか。

* ユーザーが、
  * 一つのキャッシュを保存する。
  * 一つのキャッシュを読み込む。
    * キャッシュが存在した場合は普通に読み込めるが、存在しない場合と保存されていた値と型が合わない場合の二つの異常な結果がある。
  * 一つのキャッシュを削除する。
    * キーだけを参照する。型もチェックすべきかどうか分からないが。結果は削除が成功したなら True でキャッシュが存在しなかったなら False とする。

## 2020-02-27

MonadStore についての考察。

### coding

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

```haskell
type StoreKey = [String]

data StoreValue where
  StoreValue :: (Binary a, Typeable a) => a -> StoreValue

class Monad m => MonadStore m where
  save :: StoreKey -> StoreValue -> m ()
  load :: StoreKey -> m (Maybe StoreValue)
```

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

## 2020-02-29

Store の定義が難航している。

### coding

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

## 2020-02-29

MonadStore の定義が定まった。

### coding

https://github.com/Hexirp/hexirp-hakyll/blob/3bd45ecdda2c6049d285089a377dcf2e802fb585/hexyll-core/src/Hexyll/Core/Store.hs で決定。削除はできない。どうしてもしたいなら clean コマンドを使うか、空の内容で上書きする。
