module Main where

import List

data Line = Line { number :: Int, string :: String} deriving Show
-- print はshowを継承している必要がある
myLines :: [Line]
myLines = [ (Line 4 "4th line"),
            (Line 1 "fist line"),
            (Line 5 "5th line"),
            (Line 3 "3rd line"),
            (Line 2 "second line")]

sortLines :: [Line] -> [Line]
sortLines = sortBy(\a b -> number a `compare` number b)

sortLinesByString :: [Line] -> [Line]
sortLinesByString = sortBy(\a b -> string a `compare` string b)

main::IO()
main = do print $ myLines
          print $ sortLines myLines
          print $ sortLinesByString myLines
