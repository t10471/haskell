{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Proxy
import GHC.TypeLits
import Data.Type.Bool(type (&&))
import Data.Type.Equality
import GHC.Exts(Constraint)

-- 型の関数の色々

-- 関数の型のカウント
type family Count (f :: *) :: Nat where
  Count (a -> b) = 1 + (Count b)
  Count x        = 1

type Fn1 = Int -> Int
type Fn2 = Int -> Int -> Int -> Int

fn1 :: Integer
fn1 = natVal (Proxy :: Proxy (Count Fn1))
-- 2

fn2 :: Integer
fn2 = natVal (Proxy :: Proxy (Count Fn2))

-- 型レベルリストに対する型レベルの関数

type family Reverse (xs :: [k]) :: [k] where
  Reverse '[] = '[]
  Reverse xs  = Rev xs '[]

type family Rev (xs :: [k]) (ys :: [k]) :: [k] where
  Rev '[] i       = i
  Rev (x ': xs) i = Rev xs (x ': i)

type family Length (as :: [k]) :: Nat where
  Length '[]       = 0
  Length (x ': xs) = 1 + Length xs

type family If (p :: Bool) (a :: k) (b :: k) :: k where
  If True a b  = a
  If False a b = b

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
  Concat a '[]        = a
  Concat '[] b        = b
  Concat (a ': as) bs = a ': Concat as bs

type family Map (f :: a -> b) (as :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Sum (xs :: [Nat]) :: Nat where
  Sum '[]       = 0
  Sum (x ': xs) = x + Sum xs

ex1 :: Reverse [1,2,3] ~ [3,2,1] => Proxy a
ex1 = Proxy

ex2 :: Length [1,2,3] ~ 3 => Proxy a
ex2 = Proxy

ex3 :: (Length [1,2,3]) ~ (Length (Reverse [1,2,3])) => Proxy a
ex3 = Proxy

-- 型レベルの計算を値レベルへと反映する
ex4 :: Integer
ex4 = natVal (Proxy :: Proxy (Length (Concat [1,2,3] [4,5,6])))
-- 6

ex5 :: Integer
ex5 = natVal (Proxy :: Proxy (Sum [1,2,3]))
-- 6

-- Couldn't match type ‘2’ with ‘1’
-- ex6 :: Reverse [1,2,3] ~ [3,1,2] => Proxy a
-- ex6 = Proxy

type family Elem (a :: k) (bs :: [k]) :: Constraint where
  Elem a (a ': bs) = (() :: Constraint)
  Elem a (b ': bs) = a `Elem` bs

-- 型レベルの等しさ
type family (a :: k) :=: (b :: k) :: Bool
type instance a :=: b = EqStar a b
type instance a :=: b = EqArrow a b
type instance a :=: b = EqBool a b

infixl 4 :=:

-- *同士
type family EqStar (a :: *) (b :: *) where
  EqStar a a = True
  EqStar a b = False
-- 関数同士
type family EqArrow (a :: k1 -> k2) (b :: k1 -> k2) where
  EqArrow a a = True
  EqArrow a b = False

type family EqBool a b where
  EqBool True  True  = True
  EqBool False False = True
  EqBool a     b     = False

-- 型の配列の等しさ
type family EqList a b where
  EqList '[]        '[]        = True
  EqList (h1 ': t1) (h2 ': t2) = (h1 == h2) && (t1 == t2)
  EqList a          b          = False


main :: IO ()
main = do
  putStrLn "end"
