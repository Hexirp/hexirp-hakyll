# hexirp-hakyll

hexirp-hakyll is a static web site generator library in Haskell **for Hexirp**.

hexirp-hakyll は hakyll をフォークした静的ウェブサイト生成プログラムのライブラリである。このライブラリは Hexirp が自分で利用するために制作している。

## License

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

## ライセンスの選択について

hexirp-hakyll は [hakyll](https://github.com/jaspervdj/hakyll) を `f2778e12046eb5f5eb4d377669b94b13dffc24fb` でクローンしたものである。

元々の hakyll は The 3-Clause BSD License で公開されていた。

しかし、 hakyll にはグレーな部分がある。それは GNU General Public License version 2 or later でライセンスされている pandoc に依存しているのに、本体を The 3-Clause BSD License でライセンスしていることだ。完全な違反でもない訳は、 pandoc に依存するかどうかはビルド時のフラグによって決定されるからだ。

そのため、 hexirp-hakyll は明瞭にするために、 pandoc に依存する部分を別のパッケージに分離し、 GNU General Public License version 3 or later でライセンスしている。

### pandoc のライセンスについて

事はそう単純ではなく、 pandoc は GitHub に上げられているリポジトリでは COPYRIGHT で GNU General Public License version 2 or later としてライセンスされている。その一方で Hackage では COPYING.md を参照して GNU General Public License version 2 としてライセンスされている、と表示されている。

私は Hackage を通して使う以上、それでの記述に従うべきと考える。しかし、問題なのは Hackage で配布される package のソースコードで COPYRIGHT が含まれるかどうかだ。どういうライセンスが適用されるかは、最終的にソースコードに同梱されている説明によって決められるべきであるからだ。そして、 Hackage で配布される package には COPYRIGHT が含まれていた。

そのため、私は pandoc のライセンスを GNU General Public License version 2 or later として解釈している。
