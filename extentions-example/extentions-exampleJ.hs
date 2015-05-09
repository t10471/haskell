{-# LANGUAGE PatternSynonyms #-}
module List where
import Control.Applicative
import Control.Monad.State
import Data.Foldable
import Data.Monoid
import System.Random
 
import Control.Monad.Free
 
data F m a b = F a (m b)
 
instance Functor m => Functor (F m a) where
  fmap f (F a mb) = F a (f <$> mb)
 
type List m a = Free (F m a) ()
 
pattern Nil = Pure ()
pattern Cons x mxs = Free (F x mxs)
 
-- Example from http://okmij.org/ftp/continuations/PPYield/#randoms
 
type Rand r = State r
type ListM m a = m (List m a)
 
somethingL :: RandomGen g => Int -> Int -> ListM (Rand g) Int
somethingL n m = sequenceL . replicateL n . state $ randomR (m, m+9)
 
iterateRL :: RandomGen g => ListM (Rand g) Int -> ListM (Rand g) Int
iterateRL m = appendL m (iterateRL m)
 
sequenceL :: Monad m => ListM m (m a) -> ListM m a
sequenceL mxs = do
  xs <- mxs
  case xs of
    Nil         -> return Nil
    Cons y mys  -> do
      a <- y
      return $ Cons a $ sequenceL mys
 
appendL :: Monad m => ListM m a -> ListM m a -> ListM m a
appendL mxs mys = do
  xs <- mxs
  case xs of
    Nil        -> mys
    Cons z mzs -> return $ Cons z $ appendL mzs mys
 
replicateL :: Monad m => Int -> a -> ListM m a
replicateL 0 _ = return Nil
replicateL n x = return $ Cons x $ replicateL (n-1) x
 
takeLL :: (Functor m, Monad m) => Int -> ListM m a -> m [a]
takeLL 0 _ = return []
takeLL n mxs = do
  xs <- mxs
  case xs of
    Nil        -> return []
    Cons y mys -> (y :) <$> takeLL (n-1) mys
 
headL :: Monad m => List m a -> a
headL Nil        = error "empty list"
headL (Cons x _) = x
 
run1 :: [Int]
run1 = flip evalState (mkStdGen 42) $
  takeLL 10 $
    iterateRL $ somethingL 2 0
 
run2 :: [Int]
run2 = flip evalState (mkStdGen 42) $
  takeLL 10 $ do
    iterateRL $ somethingL 2 0
    iterateRL $ somethingL 3 10
 
run3 :: [Int]
run3 = flip evalState (mkStdGen 42) $
  takeLL 10 $ do
    xs <- iterateRL (somethingL 2 0)
    iterateRL $ somethingL 3 $ headL xs

main :: IO ()
main = do
  print run1

