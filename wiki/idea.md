# idea

思い付いたアイデアを書き留めていく。

* 設定は rio で引き回そう。
* lrucache の代替ライブラリはないだろうか。たとえば mason とか。
* Metadata の仕組みと extensible パッケージは相性が良いかもしれない。
* logger や store は rio に似たパターンが使える箇所である。
* Compiler 型は Mearly マシンなどが読解の糸口にならないだろうか。
* `newtype Object f g = Object { runObject :: forall tag x. f tag -> (g tag x, x -> Object f g) }` とか。
