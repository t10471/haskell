{-# LANGUAGE FlexibleContexts #-}

import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad.List
import Control.Applicative
import Debug.Trace
import Control.Monad.Memo

data Tree a = Leaf !a | Fork !(Tree a) !(Tree a) deriving (Show,Eq)

fringe :: Tree a -> [a]
fringe (Leaf a) = [a]
fringe (Fork t u) = fringe t ++ fringe u

partitions as = [ splitAt n as | n <- [1..length as - 1 ]]

-- | Non-memoized version (Uses ListT monad - returns a list of 'Tree')
unfringe ::  (Show t) => [t] -> [Tree t]
unfringe [a] =  show [a] `trace` [Leaf a]
unfringe as  =  show as `trace` do
  (l,k) <- partitions as
  t <- unfringe l
  u <- unfringe k
  return (Fork t u)


-- | Mixes memoization with ListT monad:
-- memoizes the result as list of 'Tree' (e.g. @k :: [t]@, @v :: [Tree t]@)
unfringem :: (Ord t, Show t) => [t] -> ListT (Memo [t] [Tree t]) (Tree t)
unfringem [a] = show [a] `trace` return (Leaf a)
unfringem as = show as `trace` do
  (l,k) <- ListT $ return (partitions as)
  t <- memo unfringem l
  u <- memo unfringem k
  return (Fork t u)

evalUnfringem :: (Ord t, Show t) => [t] -> [Tree t]
evalUnfringem = startEvalMemo . runListT . unfringem

-- | Levensthein distance - recursive definition
editDistance [] ys = length ys
editDistance xs [] = length xs
editDistance (x:xs) (y:ys) 
  | x == y = editDistance xs ys
  | otherwise = minimum [
      1 + editDistance xs (y:ys),
      1 + editDistance (x:xs) ys,
      1 + editDistance xs ys]

-- | Levensthein distance - with memoization
editDistancem [] ys = return $ length ys
editDistancem xs [] = return $ length xs
editDistancem (x:xs) (y:ys) 
  | x == y = for2 memo editDistancem xs ys
  | otherwise = ((+1) . minimum) <$> sequence [
      for2 memo editDistancem xs (y:ys),
      for2 memo editDistancem (x:xs) ys,
      for2 memo editDistancem xs ys]

runEditDistancem xs ys = startEvalMemo $ editDistancem xs ys


-- | Travelling salesman problem
tsp gph mp t ss
    | ss == (mp ! t) = return (gph ! (1,t))
    | otherwise = do
  krs <- mapM (\k -> for2 memo (tsp gph mp) k ss' >>= \r -> return (k,r)) (elms ss')
  return $ minimum [ r + gph ! (k,t) | (k,r) <- krs]
   where
     ss' = ss - (mp ! t)

elms ss = go 1 ss
    where
      go b 1 = [b]
      go b ss =
          case ss `quotRem` 2 of
            (q,1) -> b : go (b+1) q
            (q,0) -> go (b+1) q

calcTsp dim =  do
  rs <- mapM (\k -> for2 memo (tsp gph mp) k (ss-1)) [2..n]
  return $ minimum [ r + gph ! (k,1) | (r,k) <- zip rs [2..n]]
    where
      n = dim^2
      cities = [(x*dim+y+1, (fromIntegral x, fromIntegral y))
                    | x <- [0..dim-1], y <- [0..dim-1]]
      dists  = [((c1,c2), sqrt ((x1-x2)^2 + (y1-y2)^2))
                    | (c1,(x1,y1)) <- cities, (c2,(x2,y2)) <- cities]
      gph = array ((1,1),(n,n)) dists               :: UArray (Int,Int) Float
      mp  = array (1,n) [(i,2^(i-1)) | i <- [1..n]] :: UArray Int Int
      ss  = 2^n-1

evalTsp = startEvalMemo . calcTsp

evalTspSTU dim = runST $ evalUArrayMemo (calcTsp dim) ((1,1),(n,2^n-1))
    where n = dim^2

evalTspIOU :: Int -> IO Float
evalTspIOU dim = evalUArrayMemo (calcTsp dim) ((1,1),(n,2^n-1))
    where n = dim^2


main :: IO ()
main = do
  putStrLn "end"
