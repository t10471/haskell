{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}

-- 2よりも複雑なサンプル

data Color = Red | Black
data Nat = Z | S Nat

data Tree :: * -> Color -> Nat -> * where
  Leaf  :: Tree a Black Z
  NodeR :: a -> Tree a Black n -> Tree a Black n -> Tree a Red   n
  NodeB :: a -> Tree a c n     -> Tree a c' n    -> Tree a Black (S n)
-- エラーになる
 -- unbalanced = NodeB () Leaf (NodeB () Leaf Leaf)
