module Etc where

-- 直積
data Product a b = Product a b
p1 (Product a b) = a
p2 (Product a b) = b
-- xにfとgを合成し、積に変形している
u f g x = Product (f x) (g x)

-- 直和
data Coproduct a b = L a | R b
q1 a = L a
q2 b = R b
u' f g x = case x of L a -> f a; R b -> g b


--代数的データ型として自然数を定義する場合
-- data Nat = Zero | Succ Nat deriving (Show,Eq)
-- 
-- foldN :: (a -> a) -> a -> Nat -> a
-- foldN f c = u
--   where
--   u Zero = c
--   u (Succ n) = f (u n)

-- 組み込みの整数を使った場合
foldN :: (a -> a) -> a -> Int -> a
foldN f c = u
  where
  u 0 = c
  u n = f (u (n-1))


-- 反射則より(xが自然数なら)
-- x == foldN (+ 1) 0 x
-- が成立します。

-- foldNでの他の関数の実装例
add x = foldN (+ 1) x
mul x = foldN (+ x) 0
pow x = foldN (* x) 1

-- 漸化式 a(n+1) = f(a(n)), a(0) = c
-- の一般解がfoldN c fとなります。
sqrt2 = foldN (\x -> (x+2/x)/2) 2

-- a(n+1) = (n+1)*a(n), a(0) = 1
-- の様なより一般的な漸化式は、foldNのみでは書けないですが、
-- fst . foldN f cの形に書くことが可能です。
fact = fst . foldN (\(c,n) -> (c*n, n+1)) (1, 1)



--代数的データ型としてリストを定義する場合
--data List a = Nil | Cons a (List a) deriving (Show,Eq)
--
--foldr :: (a -> b -> b) -> b -> List a -> b
--foldr f c = u
--  where
--  u Nil = c
--  u (Cons a as) = f a (u as)

-- 組み込みのリストを使った場合
lfoldr :: (a -> b -> b) -> b -> [a] -> b
lfoldr f c = u
  where
  u [] = c
  u (a:as) = f a (u as)

-- 反射則より
-- x == foldr (:) [] x
-- が成立します。

-- foldrでの他の関数の実装例
llength = lfoldr (\_ x -> x + 1) 0
lsum = lfoldr (+) 0
lproduct = lfoldr (*) 1

data BTree a = Leaf a | Node (BTree a) (BTree a) deriving (Show,Eq)

foldT :: (b -> b -> b) -> (a -> b) -> BTree a -> b
foldT f c = u
  where
  u (Leaf a) = c a
  u (Node l r) = f (u l) (u r)

-- 反射則より
-- x == foldT Node Leaf x
-- が成立します。

tsize    = foldT (+) (const 1)
tsum     = foldT (+) id
tproduct = foldT (*) id
