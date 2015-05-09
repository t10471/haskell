{-# LANGUAGE ViewPatterns #-}

import Control.Monad (when)
import Data.Function (fix)
import System.Environment (getArgs)

f :: IO ()
f = do
  (read -> n):_ <- getArgs
  flip fix (0 :: Int) $ \loop i ->
    when (i < n) $ do
      print i
      loop $ i + 1

g :: IO ()
g = do
  (read -> n):_ <- getArgs
  let loop i = when (i < n) $ do
        print i
        loop $ i + 1
  loop (0 :: Int)

h :: IO ()
h = do
  (read -> n):_ <- getArgs
  loop n (0 :: Int)
  where
    loop n i = when (i < n) $ do
      print i
      loop n $ i + 1

main :: IO ()
main = do
  f
  g
  h
  putStrLn "end"
