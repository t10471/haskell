{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

import GHC.Generics

{-
GHC.Genericsにある定義

class Generic a where
  type Rep a
  from :: a -> Rep a
  to :: Rep a -> a

class Datatype d where
  datatypeName :: t d f a -> String
  moduleName :: t d f a -> String

class Constructor c where
  conName :: t c f a -> String

-- | 和：コンストラクタ間の選択を表す
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | 積：コンストラクタに複数の引数があることを表す
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- | M1 のタグ：データ型
data D
-- | M1 のタグ：コンストラクタ
data C

-- | 種 * の定数、追加の引数、そして再帰
newtype K1 i c p = K1 { unK1 :: c }

-- | メタ情報（コンストラクタの名前など）
newtype M1 i c f p = M1 { unM1 :: f p }

-- | データ型のメタ情報を表す型シノニム
type D1 = M1 D

-- | コンストラクタのメタ情報を表す型シノニム
type C1 = M1 C
-}

-- 自動導出されるのを自分で記述した場合のサンプル
-- :kind!でRepの型族を見ることができる
-- :kind! Rep Animal
-- :kind! Rep ()
-- :kind! Rep [()]

data Animal = Dog | Cat

instance Generic Animal where
  type Rep Animal = D1 T_Animal ((C1 C_Dog U1) :+: (C1 C_Cat U1))

  from Dog = M1 (L1 (M1 U1))
  from Cat = M1 (R1 (M1 U1))

  to (M1 (L1 (M1 U1))) = Dog
  to (M1 (R1 (M1 U1))) = Cat

data T_Animal
data C_Dog
data C_Cat

instance Datatype T_Animal where
  datatypeName _ = "Animal"
  moduleName _   = "Main"

instance Constructor C_Dog where
  conName _ = "Dog"

instance Constructor C_Cat where
  conName _ = "Cat"


-- EqをGenericsを使って記述

class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

-- 和に対する相等性
instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

-- 積に対する相等性
instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2


class GEq a where
  geq :: a -> a -> Bool

  -- DefaultSignaturesを使う
  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)
