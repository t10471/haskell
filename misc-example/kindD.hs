{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (Eq)
import Data.Type.Equality
import GHC.TypeLits

-- 配列を反転させるにはn+0 = n と n + (1 + m) = 1 + (n + m)
-- を証明しなければならない

data MNat = Z | S MNat

data SMNat n where
  Zero :: SMNat Z
  Succ :: SMNat n -> SMNat (S n)

data Vec :: * -> MNat -> * where
  Nil :: Vec a Z
  Cons :: a -> Vec a n -> Vec a (S n)

instance Show a => Show (Vec a n) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

type family (m :: MNat) :+ (n :: MNat) :: MNat where
  Z :+ n = n
  S m :+ n = S (m :+ n)

-- (a ~ b) implies (f a ~ f b)
cong :: a :~: b -> f a :~: f b
cong Refl = Refl

-- (a ~ b) implies (f a) implies (f b)
subst :: a :~: b -> f a -> f b
subst Refl = id

plus_zero :: forall n. SMNat n -> (n :+ Z) :~: n
plus_zero Zero     = Refl
plus_zero (Succ n) = cong (plus_zero n)

plus_suc :: forall n m. SMNat n -> SMNat m -> (n :+ (S m)) :~: (S (n :+ m))
plus_suc Zero m     = Refl
plus_suc (Succ n) m = cong (plus_suc n m)

size :: Vec a n -> SMNat n
size Nil         = Zero
size (Cons _ xs) = Succ $ size xs

reverse :: forall n a. Vec a n -> Vec a n
reverse xs = subst (plus_zero (size xs)) $ go Nil xs
  where
    go :: Vec a m -> Vec a k -> Vec a (k :+ m)
    go acc Nil         = acc
    go acc (Cons x xs) = subst (plus_suc (size xs) (size acc)) $ go (Cons x acc) xs

append :: Vec a n -> Vec a m -> Vec a (n :+ m)
append (Cons x xs) ys = Cons x (append xs ys)
append Nil         ys = ys

vec :: Vec Int (S (S (S Z)))
vec = 1 `Cons` (2 `Cons` (3 `Cons` Nil))

test :: Vec Int (S (S (S Z)))
test = Main.reverse vec


-- 型レベル自然数ではplus_sucがエラーになる
type Zn = 0

type family Sn (n :: Nat) :: Nat where
  Sn n = n + 1

-- OK
eq_zero' :: Zn :~: Zn
eq_zero' = Refl

-- OK!
zero_plus_one' :: (Zn + 1) :~: (1 + Zn)
zero_plus_one' = Refl

-- OK!
plus_zero' :: forall n. (n + Zn) :~: n
plus_zero' = Refl

-- OK!
plus_one' :: forall n. (n + Sn Zn) :~: Sn n
plus_one' = Refl

-- ダメ。
-- plus_suc' :: forall n m. (n + (Sn m)) :~: (Sn (n + m))
-- plus_suc' = Refl

main :: IO ()
main = do
  putStrLn "end"
