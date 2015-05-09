{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

-- 添字付けされた型の族(indexed type family)、短くいうと型族(type family)
-- の説明

-- data族の説明
data family GMap k :: * -> *
data instance GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)

data family Array e
-- 上と下は同じ
-- data family Array :: * -> *

-- newtypeとの混合ができる
data family T a
data    instance T Int  = T1 Int | T2 Bool
newtype instance T Char = TC Bool

-- GADTsとの併用
data family G a b
data instance G [a] b where
  G1 :: c -> G [Int] b
  G2 :: G [a] Bool

data family X a
data instance X Int  = A
data instance X Char = B

-- aで代数なのででOKなようだが・・・
-- エラーになる
-- foo :: X a -> Int
-- foo A = 1             
-- foo B = 2             
-- 以下のようにしなければならない
class Foo a where
  foo :: X a -> Int
instance Foo Int where
  foo A = 1
instance Foo Char where
  foo B = 2

exX = do
  print $ foo A
  print $ foo B

-- シノニム族(型族)

type family Elem c :: *
-- 上と同じ省略できる
-- type family Elem c
type instance Elem [e] = e

type family F a b :: * -> *
{-
F Char [Int]       -- 正 種: * -> *
F Char [Int] Bool  -- 正 種: *
F Bool             -- 誤 適用が飽和していない
KindとしてはOKだけど、(:K F Bool :: * -> * -> *)
実際に使うときにはだめってこと？
F IO Bool          -- 誤 最初の引数で種が不一致
IO :: * -> * なのでだめ、どこにIOを持ってきてもだめなよう
F Bool IO もだめ
-}


type family FA a :: *
type instance FA [Int]              = Int         -- OK!
type instance FA String             = Char        -- OK!
{-
type instance FA (FA a)             = a           -- 誤: 型パラメタが型族に言及している
type instance FA (forall a. (a, b)) = b           -- 誤: 型パラメタの中にforall型がある
type instance FA Float              = forall a.a  -- 誤: 右辺がforall型であってはならない
-}

type family FB a b :: * -> *
type instance FB Int Int        = Maybe   -- OK 
{-
type instance FB Int            = (,)     -- 誤: 引数は二つでないといけない
type instance FB Int Char Float = Double  -- 誤: 引数は二つでないといけない
-}

type family FC a :: *
type instance FC (a, Int) = [a]
type instance FC (Int, a) = [a]   -- 重複が許される
{-
type family FD a :: *
type instance FD (a, Int)  = [a]
type instance FD (Char, a) = [a]  -- 非合法な重複、[Char] /= [Int]であるため
-}

-- 関連データ族と関連型族

-- class宣言の中
class GMapKey k where
  data GM k :: * -> *

class Collects ce where
  type EM ce :: *

-- クラスパラメタと全て一致する必要はない
class AC a b c where
  type ACT c a x :: *

-- instance 宣言の中
instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GM (Either a b) v = GMEither (GM a v) (GM b v)

instance (Eq (EM [e])) => Collects ([e]) where
  type EM [e] = e

-- 単一のインスタンス宣言の中に、ある関連族のインスタンスが複数あってもよい
instance GMapKey Int where
  data GM Int [v] = GM1 v
  data GM Int Int = GM2 Int

-- デフォルトを定義できる
class IsBoolMap v where
  type Key v
  type Key v = Int
  lookupKey :: Key v -> v -> Maybe Bool
-- 記述がない場合はデフォルト
instance IsBoolMap [(Int, Bool)] where
  lookupKey = lookup

-- 重複しなければ複数のデフォルトを記述できる
class ACA a where
  type FACA a b
  type FACA a Int  = Bool
  type FACA a Bool = Int
-- 左辺に出てこない型変数は右には使えない
class CACA a b where
  data CACAT a
instance CACA [c] d where
  data CACAT [c] = MkT c
  -- 「d」はスコープにない
  -- data CACAT [c] = MkT (c, d)    

type family FX a
type instance FX Bool = Int

class CX a

instance CX Int
-- 型族はインスタンス宣言には使えない
-- instance CX (FX a)
