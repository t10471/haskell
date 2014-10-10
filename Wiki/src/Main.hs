module Main where

import TextUtils
import Data.List
import Data.Maybe
import Debug.Trace


-- main = print $ lines l
-- main =  print $ concatMap (enumFromTo 1) [1,3,5]
-- main = main = print $ concatMap  dropComment  (lines l)
-- print $ categorize $ concatMap parseLine $ lines l
-- print $  map reduce $ categorize $ concatMap parseLine $ lines l


--  sum [x | x <- [1..999], mod x 3==0 || mod x 5==0]
{-
main=putStrLn $ show problem
problem = sum $ filter even $ takeWhile ( < 4000000) fib
fib = 1 : 2 : zipWith (+) fib (tail fib)
-}
type Config = [(String, String)]
l :: [Char]
l = "#aaa=1\nbbbb.c=2\nbbbb.f=7\ncccc.e=3\n#ddddd=4"

infixr 0 .$.
(.$.) :: Show a => (a -> b) -> a -> b
f .$. x = trace (show x) f x

infixr 9 ...
(...) :: Show b => (b -> c) -> (a -> b) -> (a -> c)
f ... g = (f .$.) . g
{-
main::IO()
main = print $ map reduce $ categorize $ concatMap parseLine $ lines l
  where
    reduce :: Config -> (String, Config)
    reduce kvs = (category . fst . head $ kvs,
                  map (\(k,v) -> (itemname k, v)) kvs)

    categorize :: Config -> [Config]
    categorize = groupBy (\(a,_) (b,_) -> category a == category b)

    -- a key of the config item = "category.itemname"
    category = fst . break (== '.')
    itemname = tail . snd . break (== '.')

    parseLine = parseLinePlain . dropComment

    parseLinePlain :: String -> Config
    parseLinePlain line
        | isBlank line = []
        | otherwise    = let (k, ('=':v)) = break (== '=') . strip $ line
                         in [(strip k, strip v)]

    dropComment :: String -> String
    dropComment = fst . break (== '#')
-}
{-
main::IO()
main = print $ lastButOne "abc"

lastButOne :: [a] -> a
lastButOne a = if  isMoreLength 2 a
               then a !! (length a - 2 )
               else head a
isMoreLength :: Int -> [a] -> Bool
isMoreLength n a = n < length a
-}
data List a = Cons a (List a) | Nil deriving (Show)
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: (List a) -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
-- toList $ Cons 2 (Cons 1 Nil)

data Tree a = Node a (Tree a) (Tree a)
             | Empty
               deriving (Show)

simpleTree = Node "parent" ( Node "left child" Empty Empty)
                            ( Node "left child" Empty Empty)

treeHeight Empty = 0
treeHeight (Node x left right) = 1 +  max (treeHeight left) (treeHeight right)

data MyTree a = MyTree a (Maybe (MyTree a)) (Maybe (MyTree a)) deriving (Show)

myTree = Just (MyTree "parent" (Just (MyTree "L" (Just (MyTree "LL" Nothing Nothing)) Nothing))
                          (Just (MyTree "R" (Just (MyTree "RL" Nothing Nothing)) Nothing)))

myTree2 = Just (MyTree "parent" Nothing Nothing)

treeMyHeight Nothing = 0
treeMyHeight (Just (MyTree _ l r)) = 1 + max (treeMyHeight l) (treeMyHeight r)

count :: [a] -> Int
count [] = 0
count (_:xs) = 1 + count xs

average :: [Int] -> Double
average a = (fromIntegral $ sum a) / (fromIntegral $ length a)

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs | length xs == 1 = False
                | head xs   == last xs = isPalindrome $ tail $ init xs
                | otherwise = False
subSort :: [[a]] -> [[a]]
subSort a = sortBy (\x y -> compare (length x) (length y)) a

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ []  = []
myIntersperse sep (x:xs) = x ++ concat (map (sep:) xs)

data Direction = DLeft | DRight | DStraight
  deriving (Show)
data Point = Point Double Double
  deriving (Eq, Show)

direction :: Point -> Point -> Point -> Direction
direction p1 p2 p3 | cross p1 p2 p3 < 0 = DRight
  | cross p1 p2 p3 > 0 = DLeft
  | cross p1 p2 p3 == 0 = DStraight
    where
    cross (Point x1 y1) (Point x2 y2) (Point x3 y3) =
      (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)
p1 = Point 1.0 1.0
p2 = Point 2.0 1.0
p3 = Point 1.0 3.0