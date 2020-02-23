# Hexyll.Core.Identifier.Pattern

このモジュールは、 `Identifier` 型に対するパターンマッチングを実現するためのライブラリである。

## 型の紹介

`PatternExpr` 型は、式により表され、シリアライズが可能であるパターンである。比較なども可能であり、マップやセットのメンバーになることが出来る。 `Pattern` 型と比較してほしい。

`PatternConj` 型と `PatternDisj` 型は、それぞれ論理積と論理和によるモノイドのインスタンスを実現するために、 `PatternExpr` 型をラップしたものである。中身はただのリストであるが、パターンマッチ関数によって論理積と論理和と見なされて実行される。

`Pattern` 型は、 `Identifier` 型に対するパターンを表す真の型であり、 `PatternExpr` 型などのアーキタイプである。内部は関数であるためシリアライズが出来ず、比較なども行えない。

## 内部構造

`Hexyll.Core.Identifier.Pattern.Glob` では、 `Glob` パッケージのラップを行っている。これにより `Pattern` 型が得られる。それを使って `Hexyll.Core.Identifier.Pattern(.Internal)?` では `PrimPattern` 型を構成している。これは一つ一つのパターンを表すものであり、 glob パターンと正規表現パターンとバージョンの比較パターンが使える。これらのパターンを原子命題とし、命題論理の式を組み立てるのが `PatternExpr` 型である。

## tips

パターンに `*` などをそのまま含めたければ `fromRegex` を使うとよい。 `fromGlob` では特殊な表記として扱われてしまう。さらに、 `IsString` インスタンスは一貫して `fromGlob` をベースとして作られており、 `OverloadedStrings` を使うときは `*` をそのまま使うことができない。
