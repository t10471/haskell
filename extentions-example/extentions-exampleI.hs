{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

import GHC.Exts (Constraint)
import Data.Proxy
import Data.Typeable
import Data.Data

-- as が関数の引数の型
-- a  が関数の戻り値の型
data NAry (as :: [*]) a where
  Value :: a                -> NAry '[] a
  Arg   :: (b -> NAry xs a) -> NAry (b ': xs) a

{- 上と同じ(GADTsを使わない場合)
data NAry' (as :: [*]) a = 
    (as ~ '[]) => Value' a | 
    forall b xs . (as ~ (b ': xs)) => Arg' (b -> NAry' xs a)
-}

-- 引数の数を固定しないためにどんな長さでもカインドを*とする
type family (xs :: [*]) :~> a :: *
type instance '[]       :~> a = a
type instance (x ': xs) :~> a = x -> xs :~> a
infixr :~>

-- 型リストの全てに制約をつけるための制約
type family   All (cxt :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All cxt '[]        = ()
type instance All cxt (x ': xs)  = (cxt x, All cxt xs)

-- 引数のための多相リスト
data HList (as :: [*]) where
  HNil  :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)
infixr :::
-- 型引数のための多相リスト(元記事にはなし)
data SList (as :: [*]) where
  SNil  :: SList '[]
  (:$:) :: Proxy a -> SList xs -> SList (a ': xs)
infixr :$:

-- HListを表示するため(後で付け加えたので元記事はなし)
instance (All Show as) => Show (HList as) where
  show HNil = "[]"
  show (x ::: xs) = show x ++ ":" ++ show xs
-- SListを表示するため
instance (All Typeable as, All Show as) => Show (SList as) where
  show SNil = "[]"
  show (x :$: xs) = show (typeOf x) ++ ":" ++ show xs

-- 型リストの情報のみ保持するクラス
class SingList k where
  slist :: SList k
instance SingList '[] where
  slist = SNil
instance SingList xs => SingList (x ': xs) where
  slist = (Proxy :: Proxy x) :$: slist

-- NAryから普通の関数にする
-- xs :~> a はどんな関数にもなれる
fromNAry :: NAry xs a -> xs :~> a
fromNAry (Value a) = a
fromNAry (Arg f)   = fromNAry . f

{-
型リストと関数を受け取りMAryに変換する
toNAry' ((Proxy :: Proxy Int) :$: (Proxy :: Proxy Int) :$: SNil) add
を簡約した結果が下記
Arg $                       toNAry' ((Proxy :: Proxy Int) :$: SNil) . add
Arg $ \(x :: Int ) -> Arg $ toNAry' (SNil)                          . add x
Arg $ \(x :: Int ) -> Arg $ (\(y :: Int ) -> Value y)               . add x
-}
toNAry' :: SList xs -> (xs :~> a) -> NAry xs a
toNAry' SNil         a = Value a
toNAry' (_ :$: ts)   f = Arg $ toNAry' ts . f

-- SingListを使った型の自動補完付き
toNAry :: forall xs a. SingList xs => (xs :~> a) -> NAry xs a
toNAry = toNAry' (slist :: SList xs)

-- 多相リストを適用する関数
applyHeteroList :: NAry as a -> HList as -> a
applyHeteroList (Value a) HNil          = a
applyHeteroList (Arg f)   (s ::: ts)    = applyHeteroList (f s) ts


type Readable as = All Read as
-- String 固定
applyReadList :: Readable as => NAry as a -> [String] -> Maybe a
applyReadList (Value a) []     = Just a
applyReadList (Arg f)   (x:xs) = applyReadList (f $ read x) xs
applyReadList _         _      = Nothing
-- 同じ値固定 制約は~でいける
applyHomoList :: All ((~) b) as => NAry as a -> [b] -> Maybe a
applyHomoList (Value a) []     = Just a
applyHomoList (Arg f)   (x:xs) = applyHomoList (f x) xs
applyHomoList _         _      = Nothing

add :: Int -> Int -> Int
add a b = a + b

pl :: NAry '[] Int -> NAry '[] Int -> NAry '[] Int
pl (Value a) (Value b) = Value (a + b)

main :: IO ()
main = do
  let v = Value "hello"               
  let cv = Arg $ const $ Value "hello" 
  let a = fromNAry $ Arg $ const $ Value "hello"
  print $ a 1
  let b = fromNAry $ Arg $ const $ Arg $ const $ Value "hello"
  print $ b 1 1
  let c = fromNAry $ Arg $ pl $ Value (1 :: Int)
  print $ c $ Value (1 :: Int)
  -- + だと型が曖昧なので型をInt固定にしたaddを使用
  let x = toNAry add
  let y = (1 :: Int) ::: (2 :: Int) ::: HNil
  print $ applyHeteroList x y
  putStrLn "end"
