# design

設計について。

## Log

ログを出力することが出来る文脈である。

あなたが行えること。

* あるメッセージをあるログレベルで出力する。

取り扱う対象。

* ログのメッセージ ( `LogMessage` )
* ログのレベル ( `LogLevel` )

実装上の工夫。

* ログのインデントの高さなどを `local` 関数を使って一時的に変更できる。
* `hPutBuilder` を使って出力を高速に行う。

## Store

値をファイルとして保存したりすることができる文脈である。

あなたが行えること。

* あるキーを使って、ある値を格納する。
* あるキーに対応する値を読み込む。
* あるキーに対応する値が存在するかどうか調べる。

取り扱う対象。

* キー ( `StoreKey` )
* 値 ( `StoreValue`, `exists a. (Binary a, Typeable a) => a` )

実装上の工夫。

* 読み込みの操作を `Maybe` に包むことで、値が存在するかどうか調べる操作と、値を取り出す操作を一つの関数としてまとめている。

## Provider

リソースを扱うことが出来る文脈である。

あなたが行えること。

* 全てのリソースのパスを取得する。
* リソースの直前での実行時での更新日時と現在の実行時での更新日時を取得する。
* リソースの内容を取得する。

取り扱う対象。

* リソースのパス ( `Resource` )
* リソースの直前での実行時での更新日時と現在の実行時での更新日時 ( `MTime` )
* リソースの内容 ( `Body` )

実装上の工夫。

* Store 文脈を使って、リソースの更新日時を現在の実行を超えて保存する。

## Resource

あるリソースを表す。 provider ディレクトリ以下のファイルのパスである。

リソースと一対一に対応している。コンパイルの対象となる。

## Identifier

あるコンパイル結果を表す。 `identifierPath` フィールドでどのリソースがコンパイル対象であるか示す。

あるリソースが複数の形でコンパイルされることもあり得るので、それらは `identifierVersion` フィールドで区別される。

## Metadata

あるリソースに付随するメタデータを表す。

メタデータを取り出すコンパイラーの出力である。型としては `Metadata` で表される。コンパイラーには、ファイルの先頭に書き込まれている pandoc スタイルのメタデータや、別のファイルとしてのメタデータなどに対応しているものがある。

コンパイル結果としては `identifierVersion` が `Just "metadata"` にセットされている Identifier で表される。

## Routes

コンパイル結果と出力を置くファイルパスの対応を表す。 Monoid になっている。

まだ設計が定まっておらず、現在では `Identifier -> [Path Rel File]` の newtype になっている。コンパイル結果の識別子ではなくコンパイル結果そのものを引数に取ってもいいかもしれない。

出力先が被っていた時はどうするかもまだ決めていない。警告を出すのがいいだろうと思っている。

## Writable

```haskell
class Writable a where
  write :: Handle -> a -> IO ()
```

書き込み可能で出力可能な型を表す型クラスである。 `hPutStr` や `hPut` などを抽象化するものである。

シリアライズとは違い、値の内容をそのまま出力するということが重要であり、読み込みは考慮されていない。また、 `Int` などの文字列と類似する点がない型はインスタンスになれない。 `()` がインスタンスになれるのは、空の文字列だという解釈による。

## Item

あるコンパイル結果を表す。 Identifier と似ているように見えるが、実体もついてくる。

型は自由であるが、さらに SomeItem と存在量化されることによってコンパイル結果を表す。

コンパイルの途中には同じ Identifier を持つ複数の Item が現れることがあるが、コンパイルの結果として参照されるのは最後に返される SomeItem のみである。

## Phase

コンパイルのフェーズを表す。

一つのコンパイラーを開発するときはフェーズは固定されていることが普通だと思うが、このライブラリでは自由に Compiler が作れるようになっており、その Phase も自由に設定できる。 Phase は一つのフェーズを表す型であり、それは文字列である。

## PassageMarker

あるコンパイラの進みを表すマーカーである。

Identifier には一つの Compiler が付随する。その Compiler は何々のフェーズを通過したというメッセージを出しながら進行していく。他のコンパイラーからそのメッセージを参照することができる。出したメッセージは PassageMarker という形で記録され、あるコンパイラーは PassageMarker が出力され環境に存在するまで待つことが出来る。
