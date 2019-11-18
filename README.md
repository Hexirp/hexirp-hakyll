# hexirp-hakyll

hexirp-hakyll is a static web site generator libraries in Haskell **for Hexirp**.

hexirp-hakyll は hakyll をフォークした静的ウェブサイト生成プログラムのライブラリ群である。このライブラリは Hexirp が自分で利用するために制作している。

## それぞれのライブラリについて

ここで hexirp-hakyll は長いため hexyll と略している。ライブラリを分割するわけは、ビルド時のメモリを削減するためと、コピーレフトライセンスを適用する範囲をなるべく減らすためである。

### hexyll-core

hexyll の基盤部分を記述しているパッケージである。例えば Route や Rules や Compiler などの定義を含む。

### hexyll

hexyll の本体であり Compiler や Template などの具体的な実装を含む。

## ライセンスの選択について

hexirp-hakyll は [hakyll](https://github.com/jaspervdj/hakyll) を `f2778e12046eb5f5eb4d377669b94b13dffc24fb` でクローンしたものである。

元々の hakyll は The 3-Clause BSD License で公開されていた。

しかし、 hakyll にはグレーな部分がある。それは GNU General Public License version 2 or later でライセンスされている pandoc に依存しているのに、本体を The 3-Clause BSD License でライセンスしていることだ。完全な違反でもない訳は、 pandoc に依存するかどうかはビルド時のフラグによって決定されるからだ。

そのため、 hexirp-hakyll は明瞭にするために、 pandoc に依存する部分を別のパッケージに分離し、 GNU General Public License version 3 or later でライセンスしている。

### pandoc のライセンスについて

事はそう単純ではなく、 pandoc は GitHub に上げられているリポジトリでは COPYRIGHT で GNU General Public License version 2 or later としてライセンスされている。その一方で Hackage では COPYING.md を参照して GNU General Public License version 2 としてライセンスされている、と表示されている。

私は Hackage を通して使う以上、それでの記述に従うべきと考える。しかし、問題なのは Hackage で配布される package のソースコードで COPYRIGHT が含まれるかどうかだ。どういうライセンスが適用されるかは、最終的にソースコードに同梱されている説明によって決められるべきであるからだ。そして、 Hackage で配布される package には COPYRIGHT が含まれていた。

そのため、私は pandoc のライセンスを GNU General Public License version 2 or later として解釈している。
