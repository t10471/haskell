import GetURL

import Control.Concurrent
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Our Async API:
-- Async で処理をラップ
-- <<async
data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  forkIO (do r <- action; putMVar var r)
  return (Async var)
-- 複数回呼び出されるかもしれないから、 readMVar を使う
wait :: Async a -> IO a
wait (Async var) = readMVar var
-- >>

-----------------------------------------------------------------------------

-- <<main
main = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
-- >>
