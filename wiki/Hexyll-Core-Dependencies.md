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

環境には今分かっているリソースと依存関係の二種類あるが、前者は後者のマップのキーとなっているため、一つにまとめられる。

キャッシュには、キャッシュした際のリソースのセットと依存関係のマップの二種類あるが、前者は後者のキーとなっているため、一つのマップにまとめられる。

ポイントなのは依存はパターンで表されるためパターンが一致しても依存するリソースが同じとは限らないことである。

## 内部実装

このようなモナドを使う。

```haskell
newtype DependencyEnv = DependencyEnv
  { dependencyFacts :: DependencyFacts
  } deriving (Eq, Show, Typeable)

data DependencyState = DependencyState
  { dependencyCache     :: DependencyCache
  , identifierOutOfDate :: IdentifierOutOfDate
  } deriving (Eq, Show, Typeable)

type DependencyLog = DList String

type DependencyM = RWS DependencyEnv DependencyLog DependencyState
```

実行のフェーズは三つある。

* 新しいリソースを検出する
* リソースの追加か削除で、依存するリソースのセットが変化したリソースを検出する
* 以下を古いリソースが新しく見つからなくなるまで繰り返す
  * まだ古いリソースだとされていないリソースの中から、依存しているリソースが古くなったリソースを検出する

あまり Writer モナドは使いたくないが、この場合は文字列を一つずつリストに加えていくログ用なので、評価した時に必要なメモリ数が評価しないときに必要なメモリ数とあまり変わらないと予測される。
