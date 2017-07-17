# test-extensible

Haskell の extensible パッケージのテストリポジトリ

## extensible について

以下を参照

- [extensible: Extensible, efficient, optics-friendly data types and effects | Hackage](https://hackage.haskell.org/package/extensible)
- [割とすぐに始められるextensibleチュートリアル(レコード編) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/10/10/000011)
- [波打たせるものの正体(エクステンシブル・タングル) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/12/18/181540)

## memo

* HLint とかでエラーが出てうざい
* `(>:)` で何故か怒られる
    * LTS-8.22 にある extensible のバージョンには `(>:)` は無いため
    * Nightry-2017.7.15 は最新(extensible-0.4.2)なので変更したらいけた
    * ちなみに `(>:)` は `(:>)` の型エイリアス(上記のリンク先では `(:>)` を使ってる)
    * lens の `(:>)` と被るから作ったのかな？？
* `#name` のように自動生成されるレコード関数のようなのを使うには `OverloadedLabels` が必要
    * [これ](http://d.hatena.ne.jp/kazu-yamamoto/20160114/1452735514)を自動でやってくれるのね
    * lens で `hoge ^. #name` や `view #name hoge`
* レコードのフィールドの適用順を変えてはダメ(型エラー)

### `Recod type` を追う

* `type Record = RecordOf Identity`
* `type RecordOf h = (:*) (Field h)`
* `(:*) :: (k -> *) -> [k] -> *` : Immutable product
    * `data (h :: k -> *) :* (s :: [k]) = HProduct (SmallArray# Any)`
* `SmallArray#` は Unboxed Type の配列型(`Array#` より空間が小さい？)
    * http://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:SmallArray-35
    * https://wiki.haskell.org/Unboxed_type
    * http://d.hatena.ne.jp/tanakh/?of=59
* `Any` 型
    * http://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:Any
    * http://qiita.com/lotz/items/8b22ce15fb66cf293536
* `newtype Field (h :: v -> *) (kv :: Assoc k v) = Field { getField :: h (AssocValue kv) }`
* `data Assoc k v = k :> v`
* `type family AssocValue (kv :: Assoc k v) :: v where AssocValue (k ':> v) = v`

`RecordOf h '[kv]` の `h` はモナドトランスフォーマーの基底となるモナドに近い．
`[kv]` は key-value な型レベルリスト(可変タプル)．
