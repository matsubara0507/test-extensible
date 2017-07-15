# test-extensible

Haskell の extensible パッケージのテストリポジトリ

## extensible について

以下を参照

- [extensible: Extensible, efficient, optics-friendly data types and effects | Hackage](https://hackage.haskell.org/package/extensible)
- [割とすぐに始められるextensibleチュートリアル(レコード編) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/10/10/000011)
- [波打たせるものの正体(エクステンシブル・タングル) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/12/18/181540)

## memo

* `(>:)` で何故か怒られる
    * LTS-8.22 にある extensible のバージョンには `(>:)` は無いため
    * Nightry-2017.7.15 は最新(extensible-0.4.2)なので変更したらいけた
    * ちなみに `(>:)` は `(:>)` の型エイリアス(上記のリンク先では `(:>)` を使ってる)
    * lens の `(:>)` と被るから作ったのかな？？
* `#name` のように自動生成されるレコード関数のようなのを使うには `OverloadedLabels` が必要
    * [これ](http://d.hatena.ne.jp/kazu-yamamoto/20160114/1452735514)を自動でやってくれるのね
    * lens で `hoge ^. #name` や `view #name hoge`
