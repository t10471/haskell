{-# LANGUAGE DeriveFunctor    #-}

import Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Free

data Memoize k v m a = Memoize (k -> FreeT (Memoize k v m) m v) k (v -> a)
  deriving (Functor)

type MemoT k v m = FreeT (Memoize k v m) m

runMemoT :: (Ord k, Monad m) => MemoT k v m a -> m a
runMemoT m = runFreeT m >>= flip evalStateT M.empty . run where
  run :: (Ord k1,Monad m1) =>
    FreeF (Memoize k1 v1 m1) a1 (FreeT (Memoize k1 v1 m1) m1 a1)
    -> StateT (M.Map k1 v1) m1 a1
  run (Pure x)                  = return x
  run (Free (Memoize f k cont)) = get >>= \table -> case M.lookup k table of 
    Just v -> lift . runFreeT . cont >=> run $ v
    Nothing -> do 
      v <- lift . runFreeT . f >=> run $ k
      modify $ M.insert k v
      lift . runFreeT . cont >=> run $ v
