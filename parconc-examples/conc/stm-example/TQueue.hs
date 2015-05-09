module TQueue (TQueue, newTQueue, writeTQueue, readTQueue) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, retry)

-- 2つの List を使用することで性能を向上
-- List の末尾に追加するのを避けるために
-- 読み込み書き込みのときに先頭に対して操作する
-- ただし、読み込み List が空の場合は、書き込みリストを反転させ
-- 読み込みリストに追加する
-- <<TQueue
data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry                      -- <1>
               -- let にすることで遅延実行される
               _  -> do let (z:zs) = reverse ys -- <2>
                        writeTVar write []
                        writeTVar read zs
                        return z
-- >>
