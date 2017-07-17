{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Sample.Aeson where

import           Control.Applicative (liftA2)
import           Data.Aeson          hiding (KeyValue)
import           Data.Constraint
import           Data.Extensible
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import           Data.Proxy
import           Data.String         (fromString)
import           GHC.TypeLits        (KnownSymbol, symbolVal)

instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
  parseJSON = withObject "Object" $
    \v -> hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON)) $
    \m -> let k = symbolVal (proxyAssocKey m) in
      case HM.lookup (fromString k) v of
        Just a -> Field . return <$> parseJSON a
        Nothing -> fail $ "Missing key: " `mappend` k

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
  toJSON = Object . HM.fromList . flip appEndo [] . hfoldMap getConst' . hzipWith
    (\(Comp Dict) -> Const' . Endo . (:) .
      liftA2 (,) (fromString . symbolVal . proxyAssocKey) (toJSON . getField))
    (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs)

-- instance Forall (KeyValue KnownSymbol FromJSON) xs => FromJSON (Record xs) where
--   parseJSON = withObject "Object" parser
--     where
--       parser v = hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol FromJSON)) f
--         where
--           f = g . symbolVal . proxyAssocKey
--           g k = maybe (fail' k) h $ HM.lookup (fromString k) v
--           h = fmap (Field . return) . parseJSON
--           fail' k = fail $ "Missing key: " `mappend` k

{-
  fmap :: Functor f => (a -> b) -> f a -> f b
  Field :: h (AssocValue kv) -> Field h kv
  return :: Monad m => a -> m a
  fmap Field :: Functor f => f (h (AssocValue kv)) -> f (Field h kv)
  fmap Field return :: Monad h => AssocValue kv -> Field h kv
  Field . return :: Monad h => AssocValue kv -> Field h kv
-}

-- instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Record xs) where
--   toJSON = Object . HM.fromList . flip appEndo [] . hfoldMap getConst'
--          . hzipWith . f (library :: Comp Dict (KeyValue KnownSymbol ToJSON) :* xs)
--     where
--       f (Comp Dict) v = Const' $
--         Endo ((fromString . symbolVal $ proxyAssocKey v, toJSON $ getField v) : )
