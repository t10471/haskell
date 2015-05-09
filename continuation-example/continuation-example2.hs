{-# LANGUAGE Rank2Types, ViewPatterns #-}
{-|

 Asymptotic Improvement of Computations over Free Monadsより

 通常のmonadicなコードでO(n^2)かかる計算を手でCPS変換あるいはContTモナド
 を使ってCPS変換させてO(n)に改善する。

-}
module Main where
import Control.Monad.Trans.Cont (ContT, runContT)
import Criterion.Main (defaultMain, bgroup, bench, whnf)
import Control.Monad.Trans (lift)
 
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
 
-- 木のラベルを差し替える
subst :: Tree a -> (a -> Tree b) -> Tree b
subst (Leaf a)   k = k a
subst (Node l r) k = Node (subst l k) (subst r k)
 
-- Treeとsubstはmonadになる
instance Monad Tree where
  return = Leaf
  (>>=) = subst
 
-- pred -> nはViewPatterns。元の論文ではn+kパターンを使っていた。
fullTree :: Int -> Tree Int
fullTree 1           = Leaf 1
fullTree (pred -> n) = do
  i <- fullTree n
  Node (Leaf (n - i)) (Leaf (i + 1))
 
-- 木をジグザグにたどり、葉の値を返す。zigzag . fullTreeはO(n^2)となる。
zigzag :: Tree Int -> Int
zigzag = zig
  where 
    zig (Leaf n)   = n
    zig (Node l _) = zag l
    zag (Leaf n)   = n
    zag (Node _ r) = zig r
 
-- CPS変換して高速化する
newtype CPSTree a = CPSTree (forall r. (a -> Tree r) -> Tree r)
 
rep :: Tree a -> CPSTree a
rep t = CPSTree (subst t)
 
abs :: CPSTree a -> Tree a
abs (CPSTree p) = p Leaf
 
instance Monad CPSTree where
  return a = CPSTree (\h -> h a)
  CPSTree p >>= k = CPSTree $ \h ->
                          p $ \a -> case k a of
                            CPSTree q -> q h
 
leafCPS :: a -> CPSTree a
leafCPS = return
 
nodeCPS :: CPSTree a -> CPSTree a -> CPSTree a
nodeCPS (CPSTree p1) (CPSTree p2) = CPSTree $ \h -> Node (p1 h) (p2 h)
 
improve :: CPSTree a -> Tree a
improve = Main.abs
 
-- zigzag . fullTreeCPSはO(n)
fullTreeCPS :: Int -> Tree Int
fullTreeCPS = improve . fullTree'
  where fullTree' :: Int -> CPSTree Int
        fullTree' 1           = leafCPS 1
        fullTree' (pred -> n) = do
          i <- fullTree' n
          nodeCPS (leafCPS (n - i)) (leafCPS (i + 1))
 
 
-- 同じことをContモナドで
type TreeCont a = forall r. ContT r Tree a
 
leafCont :: a -> TreeCont a
leafCont = lift . return
 
nodeCont :: TreeCont a -> TreeCont a -> TreeCont a
nodeCont l r = lift $ Node (runContT l Leaf) (runContT r Leaf)
 
-- gizgaz . fullTreeContはO(n)
fullTreeCont :: Int -> Tree Int
fullTreeCont = flip runContT return . fullTree'
  where fullTree' :: Int -> TreeCont Int
        fullTree' 1           = leafCont 1
        fullTree' (pred -> n) = do
          i <- fullTree' n
          nodeCont (leafCont (n - i)) (leafCont (i + 1))
 
-- 3種類のzigzag . fullTreeを比較する
main :: IO ()
main = defaultMain [ bgroup "Normal" [ bench (show n) $ whnf (zigzag . fullTree)     n | n <- ns]
                   , bgroup "CPS"    [ bench (show n) $ whnf (zigzag . fullTreeCPS)  n | n <- ns]
                   , bgroup "ContT"  [ bench (show n) $ whnf (zigzag . fullTreeCont) n | n <- ns]
                   ]
  where ns = take 3 [100,200..]
