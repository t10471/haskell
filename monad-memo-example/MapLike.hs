{-# LANGUAGE FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, UndecidableInstances  #-}

import Control.Monad.Memo.Class
import Control.Monad.Trans.Memo.State
import Data.MapLike
import Data.Hashable
import qualified Data.HashMap.Strict as H


fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n-1)
  f2 <- memo fibm (n-2)
  return (f1+f2)

-- Data.MapLike.InstancesにはMapとIntMapしかないので
-- 独自で作成
instance (Eq k, Hashable k) => MapLike (H.HashMap k v) k v where
    lookup = H.lookup
    add = H.insert


evalFibm :: Integer -> Integer
evalFibm n = evalMemoState (fibm n) H.empty

runFibm :: Integer -> (Integer, H.HashMap Integer Integer)
runFibm n = runMemoState (fibm n) H.empty

main :: IO ()
main = do
  print $ evalFibm 5
  putStrLn "end"
