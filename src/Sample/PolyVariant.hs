{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Sample.PolyVariant where

import           Data.Extensible
import           Data.Functor.Identity
import           Data.Maybe

---

type CardX = Variant '[ "joker" >: (), "number" >: Int ]
type InData = Variant '[ "str" >: String, "number" >: Int ]

getNumber :: (Generate xs, '["number" >: Int] ⊆ xs) => Variant xs -> Maybe Int
getNumber = matchFieldWithMaybe $ #number @= id <: nil

---

type Hoge = Variant HogeFields
type HogeFields =
  '[ "hoge" >: ()
   , "fuga" >: ()
   ]

hoge1 :: Hoge
hoge1 = embedAssoc $ #hoge @= ()

hoge2 :: Variant ("piyo" >: () ': HogeFields)
hoge2 = embedAssoc $ #piyo @= ()

hoge3 :: Variant '["hoge" >: ()]
hoge3 = embedAssoc $ #hoge @= ()

toInt1 :: (Generate xs, HogeFields ⊆ xs) => Variant xs -> Int
toInt1 = matchFieldWithDefault 0
    $ #hoge @= (const 1 :: () -> Int)
   <: #fuga @= (const 2 :: () -> Int)
   <: nil

toInt2 :: (xs ⊆ HogeFields) => Variant xs -> Int
toInt2 xs = flip matchField (spread xs :: Hoge)
    $ #hoge @= (const 1)
   <: #fuga @= (const 2)
   <: nil

matchFieldWithDefault :: forall xs ys h r .
  (Generate ys, xs ⊆ ys) => r -> RecordOf (Match h r) xs -> VariantOf h ys -> r
matchFieldWithDefault defaultValue pat =
  fromMaybe defaultValue . matchFieldWithMaybe pat

matchFieldWithMaybe :: forall xs ys h r .
  (Generate ys, xs ⊆ ys) => RecordOf (Match h r) xs -> VariantOf h ys -> Maybe r
matchFieldWithMaybe pat = matchWith func (wrench pat)
  where
    func :: forall x . Nullable (Field (Match h r)) x -> Field h x -> Maybe r
    func fx gx = (\x -> runMatch (getField x) $ getField gx) <$> getNullable fx

-- wrench pat :: Nullable (Field (Match h r)) :* ys
-- fx :: Nullable (Field (Match h r)) x

---

type KeyboardEvent = Variant KeyboardEventFields
type KeyboardEventFields =
  '[ "keyPress" >: Char
   , "keyRelease" >: Char
   ]

type MouseEvent = Variant MouseEventFields
type MouseEventFields =
  '[ "mousePress" >: (Int, Int)
   , "mouseRelease" >: (Int, Int)
   , "click" >: (Int, Int)
   ]

type Event = Variant (KeyboardEventFields ++ MouseEventFields)

getCharFromEvent ::
  (Generate xs, KeyboardEventFields ⊆ xs) => Variant xs -> Char
getCharFromEvent = matchFieldWithDefault (error "not a key")
    $ #keyPress   @= id
   <: #keyRelease @= id
   <: nil

---

type Card = Variant CardFields
type CardFields =
  '[ "number" >: Int
   , "jack"   >: ()
   , "queen"  >: ()
   , "king"   >: ()
   ]

cardNum :: Card -> Int
cardNum = matchField cardNumPattern

cardNumPattern :: RecordOf (Match Identity Int) CardFields
cardNumPattern
    = #number @= id
   <: #jack   @= const 11
   <: #queen  @= const 12
   <: #king   @= const 13
   <: nil

type CardExt = Variant CardExtFields
type CardExtFields = CardFields ++ '["joker" >: ()]

cardExtNum :: CardExt -> Int
cardExtNum =
  matchField $ shrink (#joker @= (const 0 :: () -> Int) <: cardNumPattern)

nextCardExt :: CardExt -> CardExt
nextCardExt = matchField
    $ #number @= (\n ->
        if n < 10 then embedAssoc $ #number @= n + 1 else embedAssoc $ #jack @= ()
      )
   <: #jack   @= const (embedAssoc $ #queen  @= ())
   <: #queen  @= const (embedAssoc $ #king   @= ())
   <: #king   @= const (embedAssoc $ #joker  @= ())
   <: #joker  @= const (embedAssoc $ #number @= 1)
   <: nil
