{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

import Data.Hashable
import Control.Monad.ST
import Control.Monad.Memo
import Control.Monad.Trans.Memo.ReaderCache
import qualified Data.HashTable.ST.Basic as H

newtype Container s k v = Container { toTable :: H.HashTable s k v }

type Cache s k v = ReaderCache (Container s k v)

instance (Eq k, Hashable k) => MonadMemo k v (Cache s k v (ST s)) where
        {-# INLINE memo #-}
        memo f k = do
          c <- container
          e <- lift $ H.lookup (toTable c) k
          if isNothing e
            then do
              v <- f k
              lift $ H.insert (toTable c) k v
              return v
            else return (fromJust e) 

{-# INLINE fibm #-}
fibm 0 = return 0
fibm 1 = return 1
fibm n = do
  f1 <- memo fibm (n-1)
  f2 <- memo fibm (n-2)
  return (f1+f2)

evalFib :: Int -> Int
evalFib n = runST $ do
   c <- H.new
   evalReaderCache (fibm n) (Container c)

main :: IO ()
main = do
  print $ evalFib 5
  putStrLn "end"
