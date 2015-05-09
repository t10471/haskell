import Control.Concurrent hiding (Chan, newChan, readChan, writeChan, dupChan)

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

-- <<writeChan
writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar oldHole (Item val newHole)
  putMVar writeVar newHole
-- >>

-- <<readChan
readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
  stream <- takeMVar readVar
  -- chan.hs のだとdupChanとぶつかるので
  -- take してそのままputする readMVar を使用する
  Item val tail <- readMVar stream      -- <1>
  putMVar readVar tail
  return val
-- >>

-- <<dupChan
-- write だけコピーすることにより
-- 1方に write したものを 2つで read できる
dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
  hole <- takeMVar writeVar
  putMVar writeVar hole
  newReadVar <- newMVar hole
  return (Chan newReadVar writeVar)
-- >>

-- <<unGetChan
-- read の方に挿入する
-- 空の readChan と同時に使用するとデッドロックする
-- readChan もtakeMVar で待機するため
unGetChan :: Chan a -> a -> IO ()
unGetChan (Chan readVar _) val = do
  newReadEnd <- newEmptyMVar             -- <1>
  readEnd <- takeMVar readVar            -- <2>
  putMVar newReadEnd (Item val readEnd)  -- <3>
  putMVar readVar newReadEnd             -- <4>
-- >>

main = do
  c <- newChan
  writeChan c 'a'
  readChan c >>= print
  c2 <- dupChan c
  writeChan c 'b'
  readChan c >>= print
  readChan c2 >>= print
