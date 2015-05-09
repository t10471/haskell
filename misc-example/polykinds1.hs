{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

class Category hom where
  ident :: hom a a
  compose :: hom a b -> hom b c -> hom a c

instance Category (->) where
  ident = id
  compose = flip (.)

newtype Nat f g = Nat { nu :: (forall a. f a -> g a) } 

instance Category (Nat :: (* -> *) -> (* -> *) -> *) where
  ident = Nat id
  compose f g = Nat (nu g . nu f)

class Rec hom f t where
  _in :: hom (f t) t
  out :: hom t (f t)

class HFunctor hom f where
  hmap :: hom a b -> hom (f a) (f b)

fold ::   (Category hom, HFunctor hom f, Rec hom f rec) =>   
        hom (f t) t -> hom rec t
fold   phi = compose out (compose (hmap (fold phi)) phi)
 
unfold :: (Category hom, HFunctor hom f, Rec hom f rec) => 
        hom t (f t) -> hom t rec
unfold phi = compose phi (compose (hmap (unfold phi)) _in)


data FTree a b = FLeaf a | FBranch b b
data  Tree a   =  Leaf a |  Branch (Tree a) (Tree a)

instance Rec (->) (FTree a) (Tree a) where
  _in (FLeaf a)     = Leaf a
  _in (FBranch a b) = Branch a b
  out (Leaf a)      = FLeaf a
  out (Branch a b)  = FBranch a b

instance HFunctor (->) (FTree a) where
  hmap f (FLeaf a)     = FLeaf a
  hmap f (FBranch a b) = FBranch (f a) (f b)

depth :: Tree a -> Int
depth = (fold :: (FTree a Int -> Int) -> Tree a -> Int) phi where
  phi :: FTree a Int -> Int
  phi (FLeaf a)     = 1
  phi (FBranch a b) = 1 + max a b

ex1 = print $ depth $ Branch (Leaf "Hello") (Leaf "World")

data FCTree f a = FCLeaf a | FCBranch (f (a, a))
data  CTree   a =  CLeaf a |  CBranch (CTree (a, a))

instance HFunctor Nat FCTree where
  hmap (f :: Nat (f :: * -> *) (g :: * -> *)) = Nat ff where
    ff :: forall a. FCTree f a -> FCTree g a
    ff (FCLeaf a)   = FCLeaf a
    ff (FCBranch a) = FCBranch (nu f a)

instance Rec Nat FCTree CTree where
  _in = Nat inComplete where
    inComplete  (FCLeaf a)   = CLeaf a
    inComplete  (FCBranch a) = CBranch a
  out = Nat outComplete where
    outComplete (CLeaf a)    = FCLeaf a
    outComplete (CBranch a)  = FCBranch a

data K a b = K a

cdepth :: CTree a -> Int
cdepth c = let (K d) = nu (fold (Nat phi)) c in d where
  phi :: FCTree (K Int) a -> K Int a
  phi (FCLeaf a)       = K 1
  phi (FCBranch (K n)) = K (n + 1)

ex2 = print $ cdepth $ CBranch $ CLeaf ("Hello", "World")



main :: IO ()
main = do
  putStrLn "end"

