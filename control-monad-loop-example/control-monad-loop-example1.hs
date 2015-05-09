{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Trans.Loop
import Control.Monad
import Control.Monad.IO.Class

type IntTpl = (Int, Int)

-- foreach の例
f :: IO ()
f = foreach [0..] $ \(i :: Int) -> do
  liftIO $ putStrLn $ show i ++ " loop"
  when (i == 10)        exit
  when (i `mod` 4 == 0) continue
  when (i `mod` 2 == 0) $ liftIO $ putStrLn $ show i ++ " is even"

b = do
  x <- getLine
  if x == "y"
  then return False
  else return True

-- while の例
w :: IO ()
w = while b $ do
    liftIO $ putStrLn "while"
-- doWhile の例
dw :: IO ()
dw = flip doWhile b $ do
    liftIO $ putStrLn "doWhile"
-- once の例
o :: IO ()
o = once $ do
    liftIO $ putStrLn "onece"
-- repeatLoopT の例
r :: IO ()
r = repeatLoopT $ do
    liftIO $ putStrLn "repeatLoopT"
    x <- liftIO $ getLine
    if x == "y"
    then exit
    else continue
-- iterateLoopT の例
i :: IO IntTpl
i = iterateLoopT (0,0) $ \(i,j) -> do
    liftIO $ putStrLn $ s i j 
    when (i == 10) $ exitWith (i + 10,j + 10)
    if i == 5 
    then continueWith (i + 2, j + 1)
    else continueWith (i + 1, j + 1)
  where s x y = "(" ++ show x ++ "," ++ show y ++ ")"



main :: IO ()
main = do
  f
  -- w
  -- dw
  o
  r
  (print -> x) <- i
  putStrLn "end"
