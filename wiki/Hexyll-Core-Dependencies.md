# Hexyll.Core.Dependencies

このモジュールは古いリソースを検出するためのライブラリである。

## 型

型は以下のようになる。

```haskell
-- | 依存
newtype Dependency = Dependency PatternExpr
-- | 依存関係
newtype DependencyFacts = DependencyFacts (Map Identifier [Dependency])
-- | 依存関係のキャッシュ
newtype DependencyCache = DependencyCache (Map Identifier [Identifier])
-- | 依存関係のログ
type CalculationLog = [String]

-- | 更新が必要なリソース
type IdentifierOutOfDate = Set Identifier

outOfDate
  :: IdentifierOutOfDate
  -> DependencyFacts
  -> DependencyCache
  -> (IdentifierOutOfDate, DependencyCache, CalculationLog)
```

この型は考察により得られたものである。

まず、更新が必要なリソースについての情報は必要である。今分かっているリソースについての情報も必要である。さらに、リソースの依存関係についての情報も必要である。また、以前のリソースについてのキャッシュは必要である。キャッシュにはリソースの一覧とリソースの依存関係が必要である。

ここで、リソースの依存関係の情報を記録しようとすると、必然的に今分かっているリソースについての情報が必要になるため、一つにまとめることにする。こうして二つのペアのそれぞれが一つにまとめられる。

## アルゴリズム

まず、新しく追加されたリソースと、依存しているリソースのセットが変化したリソースを out-of-date とチェックする。ここで、依存しているリソースのセットという表現なのは、リソースの一覧を必要とするリソースが存在するからである。依存関係はパターンで表されるため、依存するリソースのセットはパターンの変化がなくともリソースの追加や削除だけで変化しうる。

次に、全てのリソースのリストを取得し、それを走査して out-of-date であるリソースに依存するリソースをマークしていく。マークされたリソースはリストから取り除かれる。これを繰り返し、変化がなくなったら終了である。

## 内部実装

このようなモナドを使う。

```haskell
-- | A type of an environment for 'outOfDate'.
data DependencyEnv = DependencyEnv
  { dependencyFacts    :: DependencyFacts
  , dependencyOldCache :: DependencyCache
  } deriving (Eq, Show, Typeable)

-- | A type of a state for 'outOfDate'.
data DependencyState = DependencyState
  { dependencyNewCache  :: DependencyCache
  , identifierOutOfDate :: IdentifierOutOfDate
  } deriving (Eq, Show, Typeable)

-- | A type of a log for 'outOfDate'.
type DependencyLog = DList String

type DependencyM = RWS DependencyEnv DependencyLog DependencyState
```

あまり Writer モナドは使いたくない。だが、この場合は文字列を一つずつリストに加えていくログなので、あまり問題はないと考えられる。その理由は、評価した時に必要なメモリ数と、評価しないときに必要なメモリ数を比較すると、あまり変わらないだろうと思われるため。リファクタリング前にはリストをじかに使っていたが、この場合も必要メモリ数においては問題はなかった。問題になるのは `Sum` モノイドを使う場合などだと考えられる。それはそうとしてリストを使っていると評価時に O ( n ^ 2 ) のオーダーの時間がかかるため変更が必要だった。

`DependencyCache` が old と new で分かれているが、これは使いまわした時と比べて複数のメリットがある。

* アルゴリズムが分かりやすくなる。
    * 古いキャッシュは読むだけ、新しいキャッシュは追加するだけ、と区別が分かりやすい。
    * キャッシュに含まれているデータが古いものなのか新しいものなのか考える必要がなくなる。
    * 一つのキャッシュの上に古いデータと新しいデータが混在しない。
* 古いデータが残ったままにならない。
* `dependenciesFor` のメモ化のためのマップとして new の方を直感的に利用できる。
    * 古いデータが混ざっていると、それを区別するための仕組みが必要になる。
