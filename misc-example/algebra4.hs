{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor.Foldable
import Debug.Trace
import Control.Monad.Free
import Control.Comonad
import Control.Comonad.Cofree

{-
cata  :: Foldable t                 => (Base t a      -> a)              -> t -> a
ana   :: Unfoldable t               => (a -> Base t a)                   -> a -> t

para  :: Foldable t                 => (Base t (t, a) -> a)              -> t -> a
apo   :: (Foldable t, Unfoldable t) => (a -> Base t (Either t a))        -> a -> t

histo :: Foldable t                 => (Base t (Cofree (Base t) a) -> a) -> t -> a
futu  :: Unfoldable t               => (a   -> Base t (Free (Base t) a)) -> a -> t

hylo  :: Functor f                  => (f b -> b)      -> (a -> f a)           -> a -> b
zygo  :: Foldable t                 => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a
-}

type Nat = Fix NatF
data NatF a = S a | Z deriving (Eq,Show)

instance Functor NatF where
  fmap f Z     = Z
  fmap f (S x) = S (f x)

z :: Nat
z = Fix Z

s :: Nat -> Nat
s = Fix . S

plus :: Nat -> Nat -> Nat
plus n = cata phi where
  phi :: Base Nat Nat -> Nat
  phi Z     = n
  phi (S m) = s m

times :: Nat -> Nat -> Nat
times n = cata phi where
  phi Z     = z
  phi (S m) = plus n m

int :: Nat -> Int
int = cata phi where
  phi  Z    = 0
  phi (S f) = 1 + f

nat :: Integer -> Nat
nat = ana (psi Z S) where
  psi f _ 0 = f
  psi _ f n = f (n-1)

clength :: [a] -> Int 
clength = cata phi where
  phi Nil        = 0
  phi (Cons a b) = 1 + b

xxx :: (Show a, Integral a) => (a -> Bool) -> (a -> Bool) -> [a] -> [a] 
xxx p q = zygo f g where
  -- f がgのxsをデータを作成
  -- aは入力配列の現在の値、bはfの結果の累積
  f Nil             = []
  f (Cons a b)
        | p a       = d a:a:b
        | otherwise = d a:b
    where d r = trace ("a " ++ show a ++ " b " ++ show b) r
  g Nil             = []
  -- gはfのデータを使用する
  -- xが入力配列の現在の値
  -- xsがfの結果
  -- ysがgの結果の累積
  g (Cons x (xs,ys)) 
        | q x       = d x:ys
        | otherwise = d ys
    where d r = trace ("x "   ++ show x  ++
                       " xs " ++ show xs ++ 
                       " ys " ++ show ys) r

map' :: (a -> a) -> [a] -> [a]
map' f = hylo g project where
  g Nil        = []
  g (Cons a x) = f a : x

findDelete :: Show a => (a -> Bool) -> [a] -> (Maybe a, [a])
findDelete p = para go where
  go Nil          = (Nothing, [])
  go (Cons a (as, res))
    | p a         = (Just a, as)
    | otherwise   = let (r, rs) = res in 
                    (r, a : rs)

ex = findDelete (\x -> mod x 3 == 0) [1,2,3,4,5,6,7,8]

dropApo :: Int -> [a] -> [a]
dropApo = curry $ apo psi where 
  psi (_,[])     = Nil
  psi (n,(x:xs)) = if n < 1 then Cons x (Left xs) 
                            else psi (n-1, xs)

drop_prop = \ n (xs::[Int]) -> dropApo n xs == drop n xs

takeAna :: Int -> [a] -> [a]
takeAna = curry $ ana psi where 
  psi (_,[])     = Nil
  psi (n,(x:xs)) = if n < 1 then Nil 
                            else Cons x (n-1, xs)

take_prop = \ n (xs::[Int]) -> takeAna n xs == take n xs

takeWhileAna :: (a -> Bool) -> [a] -> [a]
takeWhileAna p = ana psi where 
  psi []                 = Nil
  psi (x:xs) | p x       = Cons x xs
             | otherwise = Nil

dropWhilePara :: (a -> Bool) -> [a] -> [a]
dropWhilePara p = para phi where 
  phi Nil           = []
  phi (Cons x (xs,ys)) 
        | p x       = ys
        | otherwise = xs

initAna :: [a] -> [a]
initAna = ana psi where 
  psi [x]    = Nil
  psi (x:xs) = Cons x xs

at' :: [a] -> Int -> [a]
at' = curry $ futu psi where 
  psi (x:_,0)  = Cons x (Free Nil)
  psi (x:xs,n) = Cons x (Pure (xs,n-1))

lastHisto :: [a] -> a
lastHisto = histo phi where 
  phi (Cons x xs) = case unwrap xs of
                      Nil       -> x
                      Cons _ ys -> extract xs

at :: [a] -> Int -> a
at = curry $ lastHisto . uncurry at'

main :: IO ()
main = do
  putStrLn "end"
