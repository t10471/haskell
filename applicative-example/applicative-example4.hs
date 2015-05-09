{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Applicative
import Debug.Trace

-- Applicative を実装しない場合の WrapMonad の動きと
-- Applicative を実装した場合の動き
-- Tree が実装していないVersion
-- TreeA が実装してあるVersion
-- 動きは一緒

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
 
subst :: Tree a -> (a -> Tree b) -> Tree b
subst (Leaf a)   k = k a
subst (Node l r) k = Node (subst l k) (subst r k)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Monad Tree where
  return = Leaf
  (>>=) = subst

up :: Int -> Int -> Tree Int
up i j = return $ i + j

tree =  Node 
          (Node (Leaf 1) (Leaf 2))
          (Node (Leaf 3) (Leaf 4))

treep :: Tree (Int -> Int)
treep =  Node
          (Node (Leaf (+1)) (Leaf (+2)))
          (Node (Leaf (+3)) (Leaf (+4)))

ex = do
    -- print $ fmap (+1) tree
    -- print $ tree >>= up 1 >>= up 2
    let ta  = WrapMonad tree
        tap = WrapMonad treep
    print $ unwrapMonad $ tap <*> ta

-- TreeA
-- Alternative を実装するため None を追加

data TreeA a = None | LeafA a | NodeA (TreeA a) (TreeA a) deriving Show
 
substA :: TreeA a -> (a -> TreeA b) -> TreeA b
substA None        k = None
substA (LeafA a)   k = k a
substA (NodeA l r) k = NodeA (substA l k) (substA r k)

apt :: TreeA (a -> b) -> TreeA a -> TreeA b
apt None          _           = None
apt _             None        = None
apt (LeafA f)     x           = fmap f x
apt (NodeA l r)   x           = NodeA (apt l x) (apt r x) 

alt :: TreeA a -> TreeA a -> TreeA a
alt (LeafA a)   None        = LeafA a
alt (NodeA l r) None        = NodeA l r
alt None        (LeafA a)   = LeafA a
alt None        (NodeA l r) = NodeA l r

instance Functor TreeA where
  fmap f None        = None
  fmap f (LeafA a)   = LeafA $ f a
  fmap f (NodeA l r) = NodeA (fmap f l) (fmap f r)

instance Monad TreeA where
  return = LeafA
  (>>=) = substA

instance Applicative TreeA where
  pure = LeafA
  f <*> a = apt f a

instance Alternative TreeA where
  empty = None
  l <|> r = alt l r

upA :: Int -> Int -> TreeA Int
upA i j = return $ i + j

treeA =  NodeA 
          (NodeA (LeafA 1) (LeafA 2))
          (NodeA (LeafA 3) (LeafA 4))

treeAp :: TreeA (Int -> Int)
treeAp =  NodeA 
          (NodeA (LeafA (+1)) (LeafA (+2)))
          (NodeA (LeafA (+3)) (LeafA (+4)))

exA = do
    print $ fmap (+1) treeA
    print $ treeAp <*> treeA
    print $ treeA >>= upA 1 >>= upA 2

main :: IO ()
main = do
  putStrLn "end"
