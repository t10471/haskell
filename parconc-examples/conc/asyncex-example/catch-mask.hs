{-# LANGUAGE BangPatterns #-}
import System.IO
import System.IO.Error
import System.Environment
import Control.Exception as E

-- 例外ハンドラの中で再帰処理を呼ぶと、マスク状態が残ってしまう
-- cat-mask2.hs が正しい
-- <<main
main = do
  fs <- getArgs
  let
     loop !n [] = return n
     loop !n (f:fs)
        = handle (\e -> if isDoesNotExistError e
                           then loop n fs
                           else throwIO e) $
            do
               getMaskingState >>= print
               h <- openFile f ReadMode
               s <- hGetContents h
               loop (n + length (lines s)) fs

  n <- loop 0 fs
  print n
-- >>
