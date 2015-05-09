module Main where

import System
import List

main::IO()
main = do args <- getArgs
          cs <- getContents
          putStr $ fgrep (head args) cs

fgrep :: String -> String -> String
-- point free style
fgrep pattern = unlines . filter (match pattern) . lines
  where
    match :: String -> String -> Bool
    -- tails make lists pair
    -- tails "abc" ["abc", "bc", "c", ""]
    match pattern = any (pattern `isPrefixOf`) . tails

