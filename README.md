# hexirp-hakyll

hexirp-hakyll is a static web site generator library in Haskell **for Hexirp**.

hexirp-hakyll は hakyll をフォークした静的ウェブサイト生成プログラムのライブラリである。このライブラリは Hexirp が自分で利用するために制作している。

# License

hexirp-hakyll は [hakyll](https://github.com/jaspervdj/hakyll) を `f2778e12046eb5f5eb4d377669b94b13dffc24fb` でクローンしたものである。

元々の hakyll は The 3-Clause BSD License で公開されていた。

しかし、 hakyll にはライセンス違反がある。それは GNU General Public License version 2 or later でライセンスされている pandoc に依存しているのに、本体を The 3-Clause BSD License でライセンスしていることだ。

そのため、 hexirp-hakyll は問題を解決するために GNU General Public License version 3 or later でライセンスしている。

もちろん事はそう単純ではなく、 pandoc は GitHub に上げられているリポジトリでは COPYRIGHT で GNU General Public License version 2 or later としてライセンスされている。その一方で Hackage では COPYING.md を参照して GNU General Public License version 2 としてライセンスされている、と表示されている。

私は Hackage を通して使う以上、それでの記述に従うべきと考える。しかし、問題なのは Hackage で配布される package のソースコードで COPYRIGHT が含まれるかどうかだ。どういうライセンスが適用されるかは、最終的にソースコードに同梱されている説明によって決められるべきであるからだ。そして、 Hackage で配布される package には COPYRIGHT が含まれていた。

以上の理由から、私は GNU General Public License version 3 or later で、このライブラリをライセンスしている。
