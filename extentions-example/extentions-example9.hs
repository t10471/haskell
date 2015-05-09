{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Foldable (forM_)
import Data.IORef

-- type family のサンプル
-- IOStoreはIOでしか使えないが
-- type familyを使ったStoreは
-- いろんなモナドに使える

class IOStore s where
  newIO :: a -> IO (s a)
  getIO :: s a -> IO a
  putIO :: s a -> a -> IO ()

-- MVarは並行処理用のデータ入れ
instance IOStore MVar where
  newIO = newMVar
  getIO = readMVar
  putIO mvar a = modifyMVar_ mvar (return . const a)

instance IOStore IORef where
  newIO = newIORef
  getIO = readIORef
  putIO ioref a = modifyIORef ioref (const a)

type Present = String

storePresentsIO :: IOStore s => [Present] -> IO (s [Present])
storePresentsIO xs = do
  s <- newIO []
  forM_ xs $ \x -> do
    old <- getIO s
    putIO s (x : old)
  return s

exIORefIO :: IO (IORef [Present])
exIORefIO = do
    s <- storePresentsIO ["Category Theory Books"]
    return s

exMVarIO :: IO (MVar [Present])
exMVarIO = do
    s <- storePresentsIO ["Category Theory Books"]
    return s

-- type familyを使ったversion

class Store s where
  -- type familyのクローズ記法
  type StoreMonad s :: * -> *
  new :: a -> (StoreMonad s) (s a)
  get :: s a -> (StoreMonad s) a
  put :: s a -> a -> (StoreMonad s) ()

instance Store IORef where
  -- IOのkindは *-> *
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

-- STMはトランザクションを管理するモナド
-- TVarはSTM内で使用するデータの入れ物
instance Store TVar where
  type StoreMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)

storePresents :: (Store s, Monad (StoreMonad s))
              => [Present] -> (StoreMonad s) (s [Present])
storePresents xs = do
  s <- new []
  forM_ xs $ \x -> do
    old <- get s
    put s (x : old)
  return s

exIO = do
    xs <- (storePresents ["Distributed Computing Through Combinatorial Topology"] 
        :: IO (IORef [Present]))
      >>= get
    print xs

exSTM = do
    xs <- atomically $ do
      (storePresents ["Distributed Computing Through Combinatorial Topology"] 
        :: STM (TVar [Present]))
      >>= get
    print xs

main :: IO ()
main = do
  putStrLn "end"
