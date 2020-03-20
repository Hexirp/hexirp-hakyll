# hexirp-hakyll

hexirp-hakyll is static web site generator libraries in Haskell **for Hexirp**.

hexirp-hakyll は hakyll をフォークした静的ウェブサイト生成プログラムのライブラリ群である。このライブラリは Hexirp が自分で利用するために制作している。

## サポートしているGHCについて

hexirp-hexyll は複数のGHCの下でのビルドをサポートしている。現時点では、それらは GHC-8.4.x と GHC-8.6.x のみである。しかし、のちに GHC-8.8.x へのサポートを追加する予定がある。

ここで複数のGHCをサポートするだけではなく、依存ライブラリについて十分に広い範囲のバージョンをサポートする必要がある。この二つの要請を満たせる方法を私は検討した。その結果、 stack を使い、複数の resolver でビルドする方法を選択した。

checked:

* lts-12.26 (GHC-8.4.4)
* lts-13.30 (GHC-8.6.5)

## それぞれのライブラリについて

ここで hexirp-hakyll は長いため hexyll と略している。ライブラリを分割するわけは、ビルド時のメモリを削減するためと、コピーレフトライセンスを適用する範囲をなるべく減らすためである。

### hexyll-core

hexyll の基盤部分を記述しているパッケージである。例えば Route や Rules や Compiler などの定義を含む。ライセンスは **Apache-2.0** である。

### hexyll

hexyll の本体であり Compiler や Template などの具体的な実装や、ルールなどのDSLから最終的なプログラムを構成する関数を含む。ライセンスは **Apache-2.0** である。

### hexyll-pandoc

hexyll の pandoc に依存する部分を切り出したパッケージである。もっとも重要なのは pandoc による Compiler の実装を含むことであろう。ライセンスは **GPL-2.0-or-later** である。

## 依存ライブラリのバージョン指定について

依存ライブラリのバージョンの指定は、下限も上限も厳密に行う。それは base も例外ではなく、全てのライブラリにバージョンの上限が設定されている。それは、破壊的変更が発生する可能性があるバージョン未満と指定している。

これの例外が、 hexirp-hakyll 内での依存関係である。そのバージョン指定はさらに厳しく、パッチバージョンの変更しか許容しない。例えば、 `hexyll-0.1.0.0` は `hexyll-core == 0.1.0.*` というバージョン指定を持つ。

## テストについて

テストは複数の種類に分けて行っている。すなわち、単体テスト (hspec) とユニットテスト (tasty) とドキュメントテスト (doctest) である。

## ライブラリを利用するときの注意について

ファイルパスは filepath パッケージなどを使って出来るだけパス区切りがOSに依存しないようにするべきである。しかし、それが出来ない場合はスラッシュによる区切りを使うべきである。それは Posix 環境ではバックスラッシュが使えないのに対して Windows 環境ではスラッシュも使えるためである。

## ライセンスについて

全体には Apache-2.0 を適用しているが、個々のパッケージでは違うライセンスが適用されていることがあるので注意するようにしてほしい。もちろん、個々のパッケージのライセンスの方が優先される。

特に pandoc 関連はコピーレフトライセンス、つまり GPL になっているので注意してほしい。

## ライセンスの選択について

hexirp-hakyll は [hakyll](https://github.com/jaspervdj/hakyll) を `f2778e12046eb5f5eb4d377669b94b13dffc24fb` でクローンしたものである。

元々の hakyll は The 3-Clause BSD License で公開されていた。

しかし、 hakyll にはグレーな部分がある。それは GNU General Public License version 2 or later でライセンスされている pandoc に依存しているのに、本体を The 3-Clause BSD License でライセンスしていることだ。完全な違反でもない訳は、 pandoc に依存するかどうかはビルド時のフラグによって決定されるからだ。

そのため、 hexirp-hakyll は明瞭にするために、 pandoc に依存する部分を別のパッケージに分離し、 GNU General Public License version 3 or later でライセンスしている。

### pandoc のライセンスについて

事はそう単純ではなく、 pandoc は GitHub に上げられているリポジトリでは COPYRIGHT で GNU General Public License version 2 or later としてライセンスされている。その一方で Hackage では COPYING.md を参照して GNU General Public License version 2 としてライセンスされている、と表示されている。

私は Hackage を通して使う以上、それでの記述に従うべきと考える。しかし、問題なのは Hackage で配布される package のソースコードで COPYRIGHT が含まれるかどうかだ。どういうライセンスが適用されるかは、最終的にソースコードに同梱されている説明によって決められるべきであるからだ。そして、 Hackage で配布される package には COPYRIGHT が含まれていた。

そのため、私は pandoc のライセンスを GNU General Public License version 2 or later として解釈している。

## サンプルについて

リポジトリについているサンプルはないが、もともと自分で使うために作っているものなので、自分のブログで使用する予定である。

## やっていけないことについて

実行中に provider ディレクトリや store ディレクトリなどの内容が変化することは全く考慮していない。よってそのようなことが起きないようにしなければならない。
