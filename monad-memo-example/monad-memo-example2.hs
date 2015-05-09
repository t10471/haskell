{-# LANGUAGE FlexibleContexts #-}



import Control.Monad.Memo
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.ST
import Debug.Trace
import Data.Array.ST
import Data.Array.Unboxed


-- 他モナドとの合成
fibmw :: (Eq k, Num k, Show k, Num n, MonadWriter String m, MonadMemo k n m) => k -> m n
fibmw 0 = "fib: 0" `trace` tell "0" >> return 0
fibmw 1 = "fib: 1" `trace` tell "1" >> return 1
fibmw n = ("fib: " ++ show n) `trace` do
  f1 <-  fibmw (n-1)
  f2 <-  fibmw (n-2)
  tell $ show n
  return (f1+f2)

evalFibmw :: Integer -> (Integer, String)
evalFibmw = startEvalMemo . runWriterT . fibmw

t1 n = startEvalMemo . runWriterT $ fibmw n >> fibmw 1 
-- 型を指定しなければOK
-- t2 n = runWriter $ fibmw n >> fibmw 1 

runFibmw n = startRunMemo . runWriterT $ fibmw n >> fibmw 1

-- 内部のキャッシュ構造の違い
evalFibmwSTA n = runST $ evalArrayMemo (runWriterT (fibmw n)) (0,n)

evalFibmwSTV n = runST $ evalVectorMemo (runWriterT (fibmw n)) n

runFibmwST :: Integer -> ((Integer,String), Array Integer (Maybe (Integer,String)))
runFibmwST n = runST $ do
   (a,arr) <- runArrayMemo (runWriterT (fibmw n)) (0,n)
   iarr <- freeze arr
   return (a,iarr)

evalFibmwIO :: Integer -> IO (Integer, String)
evalFibmwIO n = evalArrayMemo (runWriterT (fibmw n)) (0,n)


-- Const モナドとの合成
fibmc :: (Eq k, Num k, Show k, Num n, MonadCont m, MonadMemo k n m) => k -> m n
fibmc 0 = "fib: 0" `trace` return 0
fibmc 1 = "fib: 1" `trace` return 1
fibmc n = ("fib: " ++ show n) `trace` do
  f1 <- memo fibmc (n-1)
  f2 <- callCC $ \ break -> do
          if n == 4 then break 42 else memo fibmc (n-2)
  return (f1+f2)

evalFibmc :: Integer -> Integer
evalFibmc = startEvalMemo . (`runContT`return) . fibmc
--          startEvalMemo $ runContT (fibmc n) return

runFibmc = startRunMemo . (`runContT`return) . fibmc

evalFibmcIO :: Integer -> IO Integer
evalFibmcIO n = (`evalArrayMemo`(0,n)) . (`runContT`return) . fibmc $ n
--              evalArrayMemo (runContT (fibmc n) return) (0,n)

evalFibmcST :: Integer -> Integer
evalFibmcST n = runST $ (`evalArrayMemo`(0,n)) $ (`runContT`return) $ fibmc n

-- Reader モナドとの合成
fibmr :: (Eq k, Num k, Show k, Num n, MonadMemo k n m, MonadReader n m) => k -> m n
fibmr 0 = "fib: 0" `trace` return 0
fibmr 1 = "fib: 1" `trace` return 1
fibmr 2 = "fib: 2" `trace` return 1
fibmr n = ("fib: " ++ show n) `trace` do
  p1 <- ask
  p2 <- local (const p1) $ memo fibmr (n-2)          
  f1 <- memo fibmr (n-1)
  f2 <- memo fibmr (n-2)
  return (p1+f1+f2+p2)

evalFibmr :: Integer -> Integer -> Integer
evalFibmr r = startEvalMemo . (`runReaderT` r) . fibmr
--            startEvalMemo $ runReaderT (fibmr n) r

runFibmr r = startRunMemo . (`runReaderT` r) . fibmr


fibi 0 = print 0 >> return 0
fibi 1 = print 1 >> return 1
fibi n = do
  n1 <- fibi (n-1)
  n2 <- fibi (n-2)
  let r = n1+n2
  print r >> return r

-- 動かない
fibmi 0 = print 0 >> return 0
fibmi 1 = print 1 >> return 1
fibmi n = do
  n1 <- memo fibmi (n-1)
  n2 <- memo fibmi (n-2)
  let r = n1+n2
  print r >> return r

main :: IO ()
main = do
  print $ evalFibmw 8
  putStrLn "end"
