{-# LANGUAGE FlexibleContexts #-}
{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Aug 18 14:51:56 2003>
   License:    GPL
-}

{- DESCRIPTION
Try: ./ex26
-}

import Control.Monad
import Data.Char (digitToInt)
import Control.Monad.State
import Control.Monad.Writer

type NDS a = StateT Int (WriterT [String] []) a

getDigits :: Int -> [Int]
getDigits n = let s = (show n)
              in map digitToInt s

logVal :: (MonadWriter [String] m) => Int -> m Int
logVal n = do 
    tell ["logVal: " ++ (show n)]
    return n

getLogLength :: (MonadWriter [[a]] m) => m b -> m Int
getLogLength c = do 
    (_,l) <- listen $ c
    return (length (concat l))

logString :: (MonadWriter [String] m) => String -> m Int
logString s = do 
    tell ["logString: " ++ s]
    return 0

logEach :: (Show a) => [a] -> WriterT [String] [] a
logEach xs = do 
    x <- lift xs
    tell ["logEach: " ++ (show x)]
    return x
		
addVal :: (MonadState Int m) => Int -> m ()
addVal n = do 
    x <- get
    put (x+n)

setVal :: Int -> NDS ()
setVal n = do 
    x <- lift $ logVal n
    put x

addDigits :: Int -> NDS ()
addDigits n = do 
    x <- get
    y <- lift . lift $ getDigits n
    setVal (x+y)

liftListToNDS :: [a] -> NDS a
liftListToNDS = lift . lift

main = do 
    mapM_ print $ runWriterT $ (`evalStateT` 0) $ do 
      x <- lift $ getLogLength $ logString "hello"
      addDigits x
      x <- lift $ logEach [1,3,5]
      lift $ logVal x
      liftListToNDS $ getDigits 287

