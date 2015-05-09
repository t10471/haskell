-- module Tree (
--     Tree(..) 
--   , search
--   , msearch 
--   , mLsearch
-- ) where

import Control.Monad
import Data.Char

data Tree = Empty | Tree Int Tree Tree
              deriving (Show)

-- ツリーをなぞって条件に一致した値をリストで返す。
search :: (Int -> Bool) -> Tree -> [Int]
search _ Empty = []
search p (Tree c l r)
  | p c       = search p l ++ [c] ++ search p r
  | otherwise = search p l ++  search p r

-- mzero、mplus を使うと返す型の指定で検索結果をリストで返すか、Maybe で返すか指定できる。
msearch :: MonadPlus m => (Int -> Bool) -> Tree -> m Int
msearch _ Empty = mzero
msearch p (Tree c l r)
  | p c       = msearch p l `mplus` return c `mplus` msearch p r
  | otherwise = msearch p l `mplus` msearch p r

mLsearch :: MonadPlus m => (Int -> Bool) -> [Int] -> m Int
mLsearch _ [] = mzero
mLsearch isCond (x:xs)
  | isCond x  = return x `mplus` mLsearch isCond xs
  | otherwise = mLsearch isCond xs

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

main :: IO ()
main = do
  print $ search odd   (Tree 3 (Tree 2 Empty Empty) (Tree 1 Empty Empty))    -- => [3,1]
  print $ search even  (Tree 3 (Tree 2 Empty Empty) (Tree 1 Empty Empty))    -- => [2]
  print $ search (>0)  (Tree 3 (Tree 2 Empty Empty) (Tree 1 Empty Empty))    -- => [2,3,1]
  print $ (msearch odd (Tree 3 (Tree 2 Empty Empty) (Tree 1 Empty Empty)) :: [Int])     -- => [3,1]
  print $ (msearch odd (Tree 3 (Tree 2 Empty Empty) (Tree 1 Empty Empty)) :: Maybe Int)  -- => Just 3
  -- リストで返す。
  print $ (mLsearch (==10) [0..20]  :: [Int])   -- => [10]
  print $ (mLsearch (>10)  [0..20]  :: [Int])   -- => [11,12,13,14,15,16,17,18,19,20]
  print $ (mLsearch (==99) [0..20]  :: [Int])   -- => []
  -- Maybeで返す。
  print $ (mLsearch (==10)  [0..20]  :: Maybe Int) -- => Just 10
  print $ (mLsearch (==100) [0..20]  :: Maybe Int) -- => Nothing
  print $ (mLsearch (>100)  [0..20]  :: Maybe Int) -- => Nothing
