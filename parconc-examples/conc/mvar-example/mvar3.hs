import Control.Concurrent

-- 実行時エラーになる
-- <<main
main = do
  m <- newEmptyMVar
  takeMVar m
-- >>
