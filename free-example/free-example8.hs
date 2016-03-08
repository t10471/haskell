{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad.Free
import Data.Vector

data Sorter e a = Swap   Int Int a
                | Search Int Int (e -> Bool) (Int -> a)
                | Index  Int (e -> a)

instance Functor (Sorter e) where
    fmap f (Swap   j k a)   = Swap   j k (f a)
    fmap f (Search i d p g) = Search i d p (f . g)
    fmap f (Index  i g)     = Index  i (f . g)

swap   j k   = liftF $ Swap j k ()
search i d p = liftF $ Search i d p id
index  i     = liftF $ Index i id

runSorter :: Vector a -> Free (Sorter a) () -> Vector a
runSorter v (Pure _)                = v
runSorter v (Free (Swap j k cont))  = runSorter (v // [(j, v ! k), (k, v ! j)]) cont
runSorter v (Free (Index i f))      = runSorter v $ f (v ! i)
runSorter v (Free (Search j d p f)) = runSorter v $ f (loop j) where
    loop i | p (v ! i) = i
           | otherwise = loop (i + d)

quickSortMain :: Ord a => Int -> Int -> Free (Sorter a) ()
quickSortMain begin end
    | begin >= end = return ()
    | otherwise    = do
        pivot <- mode3 <$> index begin <*> index ((begin + end) `div` 2) <*> index end
        i     <- partition pivot begin end
        quickSortMain begin i
        quickSortMain (i + 1) end
    where
        mode3 a b c | a <= b && b <= c = b
        mode3 a b c | a > b            = mode3 b a c
        mode3 a b c | b > c            = mode3 a c b

        partition pivot l r = do
            j <- search l 1    (>=pivot)
            k <- search r (-1) (<=pivot)
            if j < k
                then swap j k >> partition pivot (j + 1) (k - 1)
                else return k

quickSort :: Ord a => Vector a -> Vector a
quickSort v = runSorter v (quickSortMain 0 (Data.Vector.length v - 1))

main :: IO ()
main = do
    print $ toList $ quickSort $ fromList $ [1,5,3,2,7,44,11,4]
