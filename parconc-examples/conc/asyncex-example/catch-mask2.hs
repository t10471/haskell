{-# LANGUAGE BangPatterns #-}
import System.IO
import System.IO.Error
import System.Environment
import Control.Exception
-- catch-mask.hs の修正版
-- try を使用し、例外ハンドラの外で再帰するように変更
-- <<main
main = do
  fs <- getArgs
  let
     loop !n [] = return n
     loop !n (f:fs) = do
        getMaskingState >>= print
        r <- Control.Exception.try (openFile f ReadMode)
        case r of
          Left e | isDoesNotExistError e -> loop n fs
                 | otherwise             -> throwIO e
          Right h -> do
            s <- hGetContents h
            loop (n + length (lines s)) fs

  n <- loop 0 fs
  print n
-- >>
