{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Free
import Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer

data Memoize f k v a = Memoize (k -> f (Memoize f k v) v ) k (v -> a )
  deriving (Functor)

type Memo f k v = f (Memoize f k v)

memo f k = liftF $ Memoize f k id

runMemo :: Ord k => Memo Free k v a -> a
runMemo = flip evalState M.empty . run where
  run :: Ord k1 => Memo Free k1 v1 a1 -> State (M.Map k1 v1) a1
  run (Pure x)                  = return x
  run (Free (Memoize f k cont)) = get >>= \table -> case M.lookup k table of
    Just v -> run $ cont v
    Nothing -> do 
      v <- run (f k)
      modify $ M.insert k v
      run $ cont v

showMemo :: (Ord k, Show k, Show a, Show v) => Memo Free k v a -> String
showMemo = execWriter . (flip runStateT M.empty) . run where
  run :: (Ord k1, Show k1, Show v1, Show a1) => Memo Free k1 v1 a1 -> StateT (Map k1 v1) (Writer String) a1
  run (Pure x)                  = do 
      tell $ "ans =" ++ show x ++ "\n"
      return x -- 型調整
  run (Free (Memoize f k cont)) = get >>= \table -> case M.lookup k table of
    Just v -> do 
      tell $ "hit: memo[" ++ show k ++ "] = " ++ show v ++ "\n"
      run $ cont v
    Nothing -> do
      tell $ "calc: memo[" ++ show k ++ "]" ++"\n"
      v <- run (f k)
      tell $ "cache: memo[" ++ show k ++ "] = " ++ show v ++"\n"
      modify $ M.insert k v
      run $ cont v


fib 0 = return 0
fib 1 = return 1
fib n = do
  a <- memo fib (n-1)
  b <- memo fib (n-2)
  return $ a+b


main :: IO ()
main = do
  print $ runMemo  $ fib 100
  print $ showMemo $ fib 10
