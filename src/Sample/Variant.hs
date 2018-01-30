{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Sample.Variant where

import           Control.Lens          (view, (%~), (&), (+~), (^.))
import           Data.Extensible
import           Data.Function         (on)
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.OverloadedLabels
import           GHC.TypeLits          (KnownSymbol, symbolVal)

type Point = Record '[ "x" >: Double, "y" >: Double ]
newtype Circle =
  Circle (Record '[ "mid" >: Point, "r" >: Double ]) deriving (Show, Eq)
newtype Rect =
  Rect (Record '[ "ll" >: Point, "ur" >: Point ]) deriving (Show, Eq)

type Shape = Variant
  '[ "circle" >: Circle
   , "rect"   >: Rect
   ]

shape1 :: Shape
shape1 = embedAssoc $ #circle @= Circle
    ( #mid @= (#x @= 1.0 <: #y @= 2.0 <: nil)
   <: #r   @= 2.0
   <: nil)

shape2 :: Shape
shape2 = embedAssoc $ #rect @= Rect
    ( #ll @= (#x @= 0 <: #y @= 0 <: nil)
   <: #ur @= (#x @= 2 <: #y @= 3 <: nil)
   <: nil)

area :: Shape -> Double
area = matchField
    $ #circle @= (\(Circle s) -> pi * (s ^. #r) ^ 2)
   <: #rect   @= ((*) <$> width <*> height)
   <: nil

width, height :: Rect -> Double
width  (Rect s) = abs $ s ^. #ur ^. #x - s ^. #ll ^. #x
height (Rect s) = abs $ s ^. #ur ^. #y - s ^. #ll ^. #y

addPoint :: Point -> Point -> Point
addPoint p1 p2 = p1 & #x +~ (p2 ^. #x) & #y +~ (p2 ^. #y)

class Nudge a where
  nudge :: a -> Point -> a

instance Nudge Circle where
  nudge (Circle s) p = Circle $ s & #mid %~ (`addPoint` p)

instance Nudge Rect where
  nudge (Rect s) p = Rect $ s & #ll %~ (`addPoint` p) & #ur %~ (`addPoint` p)

-- matchField :: Field (Match Identity r) :* xs -> (Field Identity) :| xs -> r ~ (Field Identity) :| xs
-- htabulateFor :: proxy c -> (forall x. c x => Membership xs x -> h x) -> h :* xs
-- htabulateFor :: proxy c -> (forall x. c x => Membership xs x -> Field (Match Identity r) x) -> Field (Match Identity r) :* xs
-- Match :: (h x -> r) -> Match h r x
-- Field :: h (AssocValue kv) -> Field h kv
-- embed :: h x -> h |: xs
instance Forall (KeyValue KnownSymbol Nudge) xs => Nudge (Variant xs)  where
  nudge s p = flip matchField s $
    htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol Nudge)) $
      \m -> Field (Match $ EmbedAt m . Field . fmap (flip nudge p))
      -- \m -> Field (Match (\(Identity x) -> EmbedAt m (Field $ Identity $ nudge x p)))

nudge' :: Shape -> Point -> Shape
nudge' s p = flip matchField s
    $ #circle @= (\s' -> embed (#circle @= nudge s' p))
   <: #rect   @= (\s' -> embed (#rect   @= nudge s' p))
   <: nil
