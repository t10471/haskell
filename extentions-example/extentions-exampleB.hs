{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- type family のサンプル
-- 型で代数を定義
-- 実際には使えなさそうな・・・

-- 右側が存在しないdata宣言にはEmptyDataDeclsが必要
data Zero
data Succ a

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four
type Six   = Succ Five

-- 足し算
type family Add a b
type instance Add Zero a     = a
type instance Add (Succ a) b = Succ (Add a b)

-- Nested type family application
--   in the type family application: Add b (Mul a b)
-- (Use UndecidableInstances to permit this)
-- In the type instance declaration for ‘Mul’
-- 掛け算 足し算の再帰だけど
type family Mul a b
type instance Mul Zero a     = Zero
type instance Mul (Succ a) b = Add b (Mul a b)

type family Fac a
type instance Fac Zero = One
type instance Fac (Succ a) = Mul (Succ a) (Fac a)

-- (undefined :: Add One One) :: Succ (Succ Zero)
-- を 2に変換してくれる関数を定義
class N a where
  n :: a -> Int
instance N Zero where
  n _ = 0
instance N a => N (Succ a) where
  -- undefined :: a の指定にはScopedTypeVariablesが必要
  -- 再帰で 1 + 1 + ... + 0になる
  n _ = 1 + n (undefined :: a)

main :: IO ()
main = do
  print $ n (undefined :: Add One One)
  print $ n (undefined :: Mul Two Three)
  print $ n (undefined :: Fac Three)
  print $ n (undefined :: Fac Four)

