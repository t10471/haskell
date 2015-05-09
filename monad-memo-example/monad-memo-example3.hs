
import Control.Applicative
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.IO
import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad.Memo.Vector.Expandable as EV
import Control.Monad.Memo

-- 色々な内部キャッシュ

fibm :: (Eq k, Num k, Num n, MonadMemo k n m) => k -> m n
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  n1 <- memo fibm (n-1)
  n2 <- memo fibm (n-2)
  return (n1+n2)

evalFibm :: Integer -> Integer
evalFibm n = evalMemo (fibm n) M.empty

runFibm :: Integer -> (Integer, M.Map Integer Integer)
runFibm = startRunMemo . fibm

evalFibmIM :: Int -> Int
evalFibmIM n = evalMemoState (fibm n) IM.empty

evalFibmSTA :: Integer -> Integer
evalFibmSTA n = runST $ evalArrayMemo (fibm n) (0,n)

runFibmSTA :: Integer -> (Integer, Array Integer (Maybe Integer))
runFibmSTA n = runST $ do
  (a,arr) <- runArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (a, iarr)

evalFibmIOA :: Integer -> IO Integer
evalFibmIOA n = evalArrayMemo (fibm n) (0,n)

runFibmIOA :: Integer -> IO (Integer, Array Integer (Maybe Integer))
runFibmIOA n = do
  (r, arr) <- runArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (r, iarr)

evalFibmSTUA :: Int -> Int
evalFibmSTUA n = runST $ evalUArrayMemo (fibm n) (0,n)

runFibmSTUA :: Int -> (Int, UArray Int Int)
runFibmSTUA n = runST $ do
    (a,arr) <- runUArrayMemo (fibm n) (0,n)
    iarr <- freeze arr
    return (a,iarr)

evalFibmIOUA :: Int -> IO Int
evalFibmIOUA n = evalUArrayMemo (fibm n) (0,n) 

runFibmIOUA :: Int -> IO (Int, UArray Int Int)
runFibmIOUA n = do
  (r, arr) <- runUArrayMemo (fibm n) (0,n)
  iarr <- freeze arr
  return (r, iarr)

evalFibmSTV :: Int -> Integer
evalFibmSTV n = runST $ evalVectorMemo (fibm n) (n+1)

evalFibmIOV :: Int -> IO Integer
evalFibmIOV n = evalVectorMemo (fibm n) (n+1)

evalFibmIOUV :: Int -> IO Int
evalFibmIOUV n = evalUVectorMemo (fibm n) (n+1)

runFibmIOUV :: Int -> IO (Int, UV.Vector Int)
runFibmIOUV n = do
  (a, vec) <- runUVectorMemo (fibm n) (n+1)
  ivec <- UV.freeze vec
  return (a, ivec)

evalFibmSTUV :: Int -> Int
evalFibmSTUV n = runST $ evalUVectorMemo (fibm n) (n+1)

runFibmSTUV :: Int -> (Int, UV.Vector Int)
runFibmSTUV n = runST $ do
    (a,vec) <- runUVectorMemo (fibm n) (n+1)
    ivec <- UV.freeze vec
    return (a,ivec)

evalFibmSTEV :: Int -> Integer
evalFibmSTEV n = runST $ EV.startEvalVectorMemo (fibm n)

evalFibmIOEV :: Int -> IO Integer
evalFibmIOEV n = EV.startEvalVectorMemo (fibm n)

evalFibmSTEUV :: Int -> Int
evalFibmSTEUV n = runST $ EV.startEvalUVectorMemo (fibm n)

runFibmSTEUV :: Int -> (Int, UV.Vector Int)
runFibmSTEUV n = runST $ do
    (a,vec) <- EV.startRunUVectorMemo (fibm n)
    ivec <- UV.freeze vec
    return (a,ivec)

evalFibmIOEUV :: Int -> IO Int
evalFibmIOEUV n = EV.startEvalUVectorMemo (fibm n)

runFibmIOEUV :: Int -> IO (Int, UV.Vector Int)
runFibmIOEUV n = do
  (a, vec) <- EV.startRunUVectorMemo (fibm n)
  ivec <- UV.freeze vec
  return (a, ivec)

main :: IO ()
main = do
  putStrLn "end"
