{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
 
import GHC.Prim (Constraint)
--  TypeFamilies,KindSignatures,ConstraintKindsを使用
-- (cx :: * -> Constraint)  (xs :: [*])が
-- KindSignaturesでcx,xsのKindを指定している
-- All 自体がConstraintKindsで独自の制約
type family All (cx :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All cx '[] = ()
type instance All cx (x ': xs) = (cx x, All cx xs)
-- GADTs as は KindSignatures
data HList (as :: [*]) where
  Nil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)
infixr :::
-- All Showは、カインド*のリストxsに対し、
-- その全ての元がShowのインスタンスになっているという制限 
-- cxがShow (Show :: * -> Constraint)
-- asがxs ([*])
instance (All Show as) => Show (HList as) where
  show Nil = "[]"
  show (x ::: xs) = show x ++ ":" ++ show xs
 
-- 実行例
-- -- > "100" ::: 4.2 ::: 10 ::: [1..5] ::: Nil
-- -- "100":4.2:10:[1,2,3,4,5]:[]

_d :: Int -> String
_d = show

_s :: String -> String
_s = id

_f :: Float -> String
_f = show

class TextF as bs where
  textf :: HList as -> HList bs -> String
-- String はそのまま出力
instance (TextF as bs) => TextF (String ': as) bs where
  textf (x ::: xs) y' = x ++ textf xs y'
-- 関数なら適用する 
instance (TextF as bs) => TextF ((x -> String) ': as) (x ': bs) where
  textf (x ::: xs) (y ::: ys) = x y ++ textf xs ys
 
instance TextF '[] '[] where
  textf _ _ = ""

printf :: (TextF as bs) => HList as -> HList bs -> IO ()
printf a b = putStrLn $ textf a b

main = do
  printf
    ("hello" ::: "world" ::: _d ::: "\n" ::: "number:" ::: _f ::: Nil)
    ((10 :: Int) ::: (1.2402 :: Float) ::: Nil)
