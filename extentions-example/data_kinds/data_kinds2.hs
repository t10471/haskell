{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}

-- DataKindsを使用

data Color = Red | Black

data Tree :: * -> Color -> * where
  Leaf  :: Tree a Black
  NodeR :: a -> Tree a Black -> Tree a Black -> Tree a Red
  NodeB :: a -> Tree a c     -> Tree a c'    -> Tree a Black

-- エラーになる
-- foo :: Tree a Int
-- foo = undefined
