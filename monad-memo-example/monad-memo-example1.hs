{-# LANGUAGE FlexibleContexts    #-}

import Control.Monad.Memo
import Control.Applicative
import Data.Functor.Identity

-- 基本的な使い方関数の結果をメモ化
-- memo :: (k -> m v ) -> k -> m v
fibm :: (Eq n, Num n, Ord n) => n -> Memo n n n
fibm 0 = return 0
fibm 1 = return 1
fibm n = (+) <$> memo fibm (n-1) <*> memo fibm (n-2)

-- ネストした型のメモ
type MemoFib = MemoT Integer Integer
type MemoBoo = MemoT Double String
type MemoFB = MemoFib (MemoBoo Identity)

boo :: Double -> MemoFB String
boo 0 = return ""
boo n = do
  -- MemoBooはMemoFBに対して一段ネストした型なのでmemol1
  n1 <- memol1 boo (n-1)           
  -- MemoFibはMemoFBに対して現在の型なのでmemol0
  fn <- memol0 fibm2 (floor (n-1))
  return (show fn ++ n1)

fibm2 :: Integer -> MemoFB Integer 
fibm2 0 = return 0
fibm2 1 = return 1
fibm2 n = do
  -- MemoBooはMemoFBに対して一段ネストした型なのでmemol1
  l <- memol1 boo (fromInteger n)
  -- MemoFibはMemoFBに対して現在の型なのでmemol0
  fn <- memol0 fibm2 (n-1)
  f1 <- memol0 fibm2 (n-1)       
  f2 <- memol0 fibm2 (n-2)
  return (f1 + f2 + floor (read l))

evalFibM2 :: Integer -> Integer
evalFibM2 = startEvalMemo . startEvalMemoT . fibm2


-- 相互再帰
-- 通常版
f :: Int -> (Int,String)
f 0 = (1         , "+")
f n = (g(n, ff n), "-" ++ fs n)
  where ff n = fst(f(n-1))
        fs n = snd(f(n-1))
g :: (Int, Int) -> Int
g (0,m) = m           + 1
g (n,m) = fst(f(n-1)) - g((n-1),m)


-- 相互再帰
-- メモ版
type MemoF = MemoT Int (Int,String)
type MemoG = MemoT (Int,Int) Int
-- 合成
type MemoFG = MemoF (MemoG Identity)

fm :: Int -> MemoFG (Int,String)
fm 0 = return (1,"+")
fm n = do
  fn <- memol0 fm (n-1)
  gn <- memol1 gm ((n-1) , fst fn)
  return (gn , "-" ++ snd fn)

gm :: (Int,Int) -> MemoFG Int
gm (0,m) = return (m+1) 
gm (n,m) = do
  fn <- memol0 fm (n-1)
  gn <- memol1 gm ((n-1),m)
  return $ fst fn - gn

evalAll = startEvalMemo . startEvalMemoT
-- fmを実行
evalFm :: Int -> (Int, String)
evalFm = evalAll . fm
-- gmを実行
evalGm :: (Int,Int) -> Int
evalGm = evalAll . gm

-- for2を使ったVersion
fm2 :: Int -> MemoFG (Int,String)
fm2 0 = return (1,"+")
fm2 n = do
  fn <- memol0 fm2 (n-1)
  gn <- for2 memol1 gm2 (n-1) (fst fn)
  return (gn , "-" ++ snd fn)

gm2 :: Int -> Int -> MemoFG Int
gm2 0 m = return (m+1) 
gm2 n m = do
  fn <- memol0 fm2 (n-1)
  gn <- for2 memol1 gm2 (n-1) m
  return $ fst fn - gn

evalFm2 :: Int -> (Int, String)
evalFm2 = evalAll . fm2

evalGm2 :: Int -> Int -> Int
evalGm2 n m = evalAll $ gm2 n m



-- 複数引数に対応
-- 普通のアッカーマン関数
ack :: (Eq n, Num n) => n -> n -> n
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

-- メモ化したアッカーマン関数
-- fibm :: (Eq n, Num n, Ord n) => n -> Memo n n n
-- に比べて MonadMemo (n, n) n mにしているあたりが違う
ackm :: (Num n, Ord n, MonadMemo (n, n) n m) => n -> n -> m n
ackm 0 n = return (n+1)
-- for2 で引数2つの関数に対応
ackm m 0 = for2 memo ackm (m-1) 1
ackm m n = do
  n1 <- for2 memo ackm m (n-1)
  for2 memo ackm (m-1) n1

evalAckm :: (Num n, Ord n) => n -> n -> n
evalAckm n m = startEvalMemo $ ackm n m


main :: IO ()
main = do
  print $ startEvalMemo $ fibm 10
  print $ evalFibM2 5
  print $ evalAckm  2 2
  print $ evalFm 3
  print $ evalGm (3,3)
  print $ evalFm2 3
  print $ evalGm2 3 3
  putStrLn "end"
