{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

data Nat = Z | S Nat deriving (Eq, Show)

type Zero  = Z
type One   = S Zero
type Two   = S One
type Three = S Two
type Four  = S Three
type Five  = S Four

-- DataKindsを使って値コンストラクタを型コンストラクタに昇格

-- 長さの型を持つ配列

data Vec :: Nat -> * -> * where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

instance Show a => Show (Vec n a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

class FromList n where
  fromList :: [a] -> Vec n a

instance FromList Z where
  fromList [] = Nil

instance FromList n => FromList (S n) where
  fromList (x:xs) = Cons x $ fromList xs

lengthVec :: Vec n a -> Nat
lengthVec Nil         = Z
lengthVec (Cons x xs) = S (lengthVec xs)

zipVec :: Vec n a -> Vec n b -> Vec n (a,b)
zipVec Nil Nil                 = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x,y) (zipVec xs ys)

vec4 :: Vec Four Int
vec4 = fromList [0, 1, 2, 3]

vec5 :: Vec Five Int
vec5 = fromList [0, 1, 2, 3, 4]


example1 :: Nat
example1 = lengthVec vec4
-- S (S (S (S Z)))

example2 :: Vec Four (Int, Int)
example2 = zipVec vec4 vec4
-- Cons (0,0) (Cons (1,1) (Cons (2,2) (Cons (3,3) (Nil))))

-- 空かどうかを判別できるリスト

data Size = Empty | NonEmpty

data List a b where
  ENil  :: List Empty a
  ECons :: a -> List b a -> List NonEmpty a

head' :: List NonEmpty a -> a
head' (ECons x _) = x

example3 :: Int
example3 = head' (1 `ECons` (2 `ECons` ENil))

-- Cannot match type Empty with NonEmpty
-- example2 :: Int
-- example2 = head' Nil


main :: IO ()
main = do
  putStrLn "end"
