{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Sample.PolyRecordAndVariant where

import           Control.Lens          ((^.))
import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits          (KnownSymbol, symbolVal)

type R1 = Record '[ "a" >: Int, "b" >: String]
type R2 = Record '[ "a" >: Int, "c" >: Char]
type R3 = Record '[ "b" >: String ]

type R = Variant RFields
type RFields =
  '[ "r1" >: R1
   , "r2" >: R2
   , "r3" >: R3
   ]

r1, r2, r3 :: R
r1 = embedAssoc $ #r1 @= (#a @= 1 <: #b @= "hoge" <: nil)
r2 = embedAssoc $ #r2 @= (#a @= 2 <: #c @= 'c' <: nil)
r3 = embedAssoc $ #r3 @= (#b @= "fuga" <: nil)

applyField :: (Monoid r, Forall (KeyIs KnownSymbol) xs)
  => String -> (forall x. x -> r) -> Record xs -> r
applyField k f = hfoldMapWithIndexFor (Proxy :: Proxy (KeyIs KnownSymbol)) $
  \m x -> let k' = symbolVal (proxyAssocKey m) in
    if k' == k then f (runIdentity $ getField x) else mempty

applyField' :: forall r c xs proxy . (Monoid r, Forall (KeyValue KnownSymbol c) xs)
  => proxy c -> String -> (forall x. c x => x -> r) -> Record xs -> r
applyField' _ k f = hfoldMapWithIndexFor (Proxy :: Proxy (KeyValue KnownSymbol c)) $
  \m x -> let k' = symbolVal (proxyAssocKey m) in
    if k' == k then f (runIdentity $ getField x) else mempty

class PrintAField r where
  printAField :: r -> IO ()

instance Forall (KeyValue KnownSymbol Show) xs => PrintAField (Record xs) where
  printAField = applyField' (Proxy :: Proxy Show) "a" print

hoge :: R -> IO ()
hoge = matchField $ htabulateFor (Proxy :: Proxy (ValueIs PrintAField)) $
  \m -> Field (Match $ printAField . runIdentity)

-- hasAField :: Forall (KeyIs KnownSymbol) xs => Variant xs -> Bool
-- hasAField = undefined
--
-- class HasAField r where
--   printAField :: r -> IO ()
--
-- instance (Show a, Associate "a" a xs, ("a" >: a) ∈ xs) => HasAField (Record xs) where
--   printAField r = print (r ^. #a)
--
-- instance (Associate "a" Int xs) => HasAField (Record xs) where
--   printAField r = print (r ^. #a)
--
-- instance {-# OVERLAPPABLE #-} HasAField (Record xs) where
--   printAField _ = pure ()
--
-- f :: R -> IO ()
-- f = matchField r
--   where
--     r :: RecordOf (Match Identity (IO ())) RFields
--     r = htabulateFor
--       (Proxy :: Proxy (ValueIs HasAField))
--       (\m -> Field (Match $ printAField . runIdentity))
--
-- findR :: FieldName k -> Record xs -> Maybe v
-- findR k r =
