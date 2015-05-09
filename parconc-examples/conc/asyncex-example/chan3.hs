import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, dupChan)
import Control.Exception

-- <<Stream
type Stream a = MVar (Item a)
data Item a   = Item a (Stream a)
-- >>

-- <<Chan
data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))
-- >>

-- <<newChan
newChan :: IO (Chan a)
newChan = do
  hole  <- newEmptyMVar
  readVar  <- newMVar hole
  writeVar <- newMVar hole
  return (Chan readVar writeVar)
-- >>

-- <<wrongWriteChan
wrongWriteChan :: Chan a -> a -> IO ()
wrongWriteChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  modifyMVar_ writeVar $ \oldHole -> do
    putMVar oldHole (Item val newHole)  -- <1>
    -- ここで割り込みが発生する可能性があるので正しくない
    return newHole                      -- <2>
-- >>

-- <<writeChan
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  -- 全体を マスクするしかない
  -- 2つの putMVar はブロックする処理だが、空であることが保証されている
  -- のでブロックはされない
  mask_ $ do
    oldHole <- takeMVar writeVar
    putMVar oldHole (Item val newHole)
    putMVar writeVar newHole
-- >>

-- <<readChan
-- modifymvar を使うように変更
-- readMVar 内でも mask を使用している
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  modifyMVar readVar $ \stream -> do
    Item val tail <- readMVar stream
    return (tail, val)
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
