-- <<fork
import Control.Concurrent
import Control.Monad
import System.IO

main = do
  -- バッファなしでスレッドの交錯をはっきりさせる
  hSetBuffering stdout NoBuffering            -- <1>
  -- replicateM_ :: Monad m => Int -> m a -> m ()
  -- Int 回繰り返す。結果の戻り値を無視する
  -- 別スレッド
  forkIO (replicateM_ 100000 (putChar 'A'))   -- <2>
  -- メインスレッド
  replicateM_ 100000 (putChar 'B')            -- <3>
-- >>
