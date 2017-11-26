# test-extensible

Haskell の extensible パッケージのテストリポジトリ

## extensible について

以下を参照

- [extensible: Extensible, efficient, optics-friendly data types and effects | Hackage](https://hackage.haskell.org/package/extensible)
- [割とすぐに始められるextensibleチュートリアル(レコード編) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/10/10/000011)
- [波打たせるものの正体(エクステンシブル・タングル) - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2016/12/18/181540)
- [ぼくのかんがえた最強の拡張可能レコード - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2015/01/21/175227)
     - 少し古いけど分かりやすい

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

### `map` のようなことをするには？

[Data.Extensible.Class](https://hackage.haskell.org/package/extensible-0.4.2/docs/Data-Extensible-Class.html)の [`Forall`](https://hackage.haskell.org/package/extensible-0.4.2/docs/Data-Extensible-Class.html#t:Forall) を使う．

```haskell
class (ForallF c xs, Generate xs) => Forall (c :: k -> Constraint) (xs :: [k]) where
  henumerateFor :: proxy c -> proxy' xs -> (forall x. c x => Membership xs x -> r -> r) -> r -> r
  hgenerateListFor :: Applicative f
    => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (HList h xs)
```


* `Forall` 型クラスは，特定の型レベルリストに型レベルの繰り返し文を与えるような型クラス
    * インスタンスは単純に空の型レベルリストと，そうでない場合の場合分け(たぶん)  
* `henumerateFor` は列挙する関数(？)
    * `proxy c` は型レベルリストへの制約の型
        * [Constraint型族](https://github.com/shiatsumat/wiwinwlh-jp/wiki/%E5%9E%8B%E6%97%8F#constraint-kinds)
    * `proxy' xs` は型レベルリストの型
    * `Membership xs x` 型は型レベルセット(リスト)の要素型 `x` の `xs` での位置を表すらしい
* `hgenerateListFor` はへテロリストの生成関数(？)
    * `h` はモナドみたいなもの

繰り返しには以下の関数を使う

```haskell
-- Data.Extensible.Product
hgenerateFor :: (Forall c xs, Applicative f)
  => proxy c -> (forall x. c x => Membership xs x -> f (h x)) -> f (h :* xs)
hgenerateFor p f = fmap fromHList $ hgenerateListFor p f
```

* `p` は制約
* `f` は繰り返し処理する関数(？)

例えば，`FromJSON`

```haskell
instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $
    \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON)) $
    \m -> let k = symbolVal (proxyAssocKey m) in
      case HM.lookup (fromString k) v of
        Just a -> Field . return <$> parseJSON a
        Nothing -> fail $ "Missing key: " `mappend` k
```

(型が曖昧になってしまうせいか，うまく補助関数は使えない)

* `withObject "Object"` ってのはタダの aeson の関数
    * `parseJSON (Object v) = ...` と同じ
    * マッチしなかったときの処理を規定してくれる
* `Proxy :: Proxy (KeyValue KnownSymbol FromJSON)` が制約
    * `KnownSymbol` が Key への制約
        * [`KnownSymbol`](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-TypeLits.html#t:KnownSymbol) は型レベル文字列であることを指してる
    * `FromJSON` が Value への制約
        * 要するに Value の型は `FromJSON` 型クラスのインスタンスであるって意味
* 今回 `m` は Key-Value な一要素と考えれば良さそう
    * [`proxyAssocKey`](https://hackage.haskell.org/package/extensible-0.4.2/docs/Data-Extensible-Field.html#v:proxyAssocKey)は型レベルKey-Valueから型レベル文字列(Key)を取り出す
    * [`symbolVal`](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-TypeLits.html#v:symbolVal)は型レベル文字列から文字列に変換する
    * `fromString` は単純に文字列型から `Text` や `ByteString` のような `IsString` 型クラスのインスタンスの型に変換する関数
        * aeson の `Object` 型は `HashMap Text Value` のため
    * `Field . return :: Monad h => AssocValue kv -> Field h kv`

`ToJSON` の場合

```haskell
instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
  toJSON = Object . HM.fromList . flip appEndo [] . hfoldMap getConst' . hzipWith
    (\(Comp Dict) -> Const' . Endo . (:) .
      liftA2 (,) (fromString . symbolVal . proxyAssocKey) (toJSON . getField))
    (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs)
```

* `HM.fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v`
* [`Endo`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Monoid.html#t:Endo)
    * `newtype Endo a = Endo { appEndo :: a -> a }`
    * ref: [Data.Monoidを眺めた·うさぎ小屋](https://kimiyuki.net/blog/2014/12/16/about-data-monoid/)
    * e.g ` (appEndo . mconcat . map Endo) [(+ 1), (* 4), (+ 3)] 0 = (0 + 3) * 4 + 1 = 13`
* `hzipWith :: (forall x. f x -> g x -> h x) -> (f :* xs) -> (g :* xs) -> h :* xs` : zipWith のへテロリスト版
* `hfoldMap :: Monoid a => (forall x. h x -> a) -> (h :* xs) -> a` : `map` して `fold` のへテロリスト版
* `newtype Const' a x = Const' { getConst' :: a }` : `const` 関数の型バージョン
* `newtype Comp (f :: j -> *) (g :: i -> j) (a :: i) = Comp { getComp :: f (g a) }` : 関数合成を模した型
* `library :: forall c xs. Forall c xs => Comp Dict c :* xs` : 制約辞書のへテロリスト
    * `data Dict :: Constraint -> *`
    * `(KeyValue KnownSymbol ToJSON) :: kv -> Constraint` かな？
    * `(Comp Dict (KeyValue ...) :: kv -> *) :* xs` って区切り方
    * イメージとしては `[Comp Dict]` な感じ(リストじゃなくてヘテロリストもとい Record だけど)
* zip する関数
    * `toJSON . getField $ v` でフィールドの値を JSON にしてる
    * `fromString . symbolVal . proxyAssocKey` で型レベルに存在するフィールドの Key を取得し，文字列にまで変換
    * これらをタプルに `(k, v)`
    * `(:) (k, v)` とすることで `[(k,v)] -> [(k,v)]` の型になる
    * コレを `Endo` 型でラップし，さらに `Const'` 型でラップ
    * `Const'` は `hzipWith` の型と整合性を保つため
         * `(forall x. f x -> g x -> h x)` の部分で，`x` はフィールドだと思えばいい
* `Endo ([(k,v)] -> [(k,v)])` 型のラップした関数だけ `getConst'` で取り出し，`Endo` 型を畳み込む(関数合成)
* `flip appEndo []` で空のリストに適用し，`HM.fromList` でハッシュマップに変換し，`Object` でラップ   

### cassave の Csv をやってみる

* `ToJSON` と `FromJSON` をそのままで結構できる
* ただし，Header の無い CSV からの変換対応型クラス `FromCsv` はお手上げ
    * レコードに依存しないやり方が分からない
    * `[Int]` と `zip` みたいなことしたいけど，実体のない状態ではできそうにない...
* `Forall (KeyValue c1 c2) xs` の `c1` や `c2` に何でもいいという制約はどうやって渡すんだろうか？？

```haskell
>> import Data.Csv
>> import Data.Vector
>> :t either error snd $ decodeByName $ encodeByName (headerOrder book1) [book1,book2] :: Vector Book
either error snd $ decodeByName $ encodeByName (headerOrder book1) [book1,book2] :: Vector Book
  :: Vector Book
>> either error snd $ decodeByName $ encodeByName (headerOrder book1) [book1,book2] :: Vector Book
[name @= "Type and Programming Language" <: author @= ["Benjamin C. Pierce"] <: date @= "January 2002" <: isbm @= "9780262162098" <: price @= 95.0 <: nil,name @= "Structure and Interpretation of Computer Programs" <: author @= ["Harold Abelson","Gerald Jay Sussman","Julie Sussman"] <: date @= "July 1996" <: isbm @= "9780262510875" <: price @= 55.0 <: nil]
``` 

### Tangle !!!

- Monadic な処理でフィールドを構築したいとき
    - フィールド `("hoge" >: Hoge)` 用の型クラスを定義して各フィールドのインスタンスを作る
    - それを `hgenerateFor` して合わせる
    - `<@=>` でも良さそう
- さらにフィールドごとに依存関係が欲しいとき
    - **Tangle を使う！**
    - `lesso` 関数でフィールドの値を呼び出せる
- このあたりには `PolyKinds` 拡張が要る

### Variant 

[拡張可能な直和型っぽい](https://hackage.haskell.org/package/extensible/docs/Data-Extensible-Sum.html)

```haskell
>> type Color = Variant '[ "rgb" >: (Int,Int,Int), "cmyk" >: (Int,Int,Int,Int) ]
>> color1 = embed $ #rgb @= (0 :: Int, 0 :: Int, 0 :: Int) :: Color
>> color1
EmbedAt $(mkMembership 0) (rgb @= (0,0,0))
>> color2 = embedAssoc $ #cmyk @= (0,0,0,0) :: Color
>> color2
EmbedAt $(mkMembership 1) (cmyk @= (0,0,0,0))
```

`embed $ #rgb @= (0,0,0) :: Color` ってできないのは `0` が `Integer` になるから？？
こんなエラーが出る

```Haskell
>> color1 = embed $ #rgb @= (0,0,0) :: Color

<interactive>:29:10: error:
    ? Couldn't match type ‘'Missing
                             ("rgb" ':> (Integer, Integer, Integer))’
                     with ‘'Expecting pos0’
        arising from a use of ‘embed’
    ? In the expression: embed $ #rgb @= (0, 0, 0) :: Color
      In an equation for ‘color1’:
          color1 = embed $ #rgb @= (0, 0, 0) :: Color
```

### Inclution

`xs ⊆ ys` が[定義されてまして](https://hackage.haskell.org/package/extensible/docs/Data-Extensible-Inclusion.html)，おそらく「`ys` は `xs` が持つフィールドをすべて持ってる」って感じの意味だと思う．
その型クラスに関する関数に `shrink` と `spread` がありまして，`shrink` は直積型を，`spread` は直和型をまさに拡張可能に変換してくれる(笑)

```haskell
>> :set -XOverloadedLabels -XOverloadedStrings -XDataKinds -XTypeOperators
>> :m Data.Extensible Data.Text
>> type A = Record '[ "name" >: Text, "age" >: Int ]
>> type B = Record '[ "name" >: Text, "age" >: Int, "isHuman" >: Bool ]
>> person1 = #name @= "Alise" <: #age @= 18 <: emptyRecord :: A
>> person2 = #name @= "Alise" <: #age @= 18 <: #isHuman @= True <: emptyRecord :: B
>> person1' = shrink $ person2 :: A
>> person1
name @= "Alise" <: age @= 18 <: nil
>> person2' = shrink $ #isHuman @= True <: person1 :: B
>> person2'
name @= "Alise" <: age @= 18 <: isHuman @= True <: nil
```

`Color` 型は Variant のときのを使ってる

```haskell
>> type RGB = Variant '[ "rgb" >: (Int,Int,Int) ]
>> type CMYK = Variant '[ "cmyk" >: (Int,Int,Int,Int) ]
>> color3 = embed $ #rgb @= (0 :: Int, 0 :: Int, 0 :: Int) :: RGB
>> color4 = embedAssoc $ #cmyk @= (0,0,0,0) :: CMYK
>> color1' = spread color3 :: Color
>> color2' = spread color4 :: Color
```

すごい(笑)
