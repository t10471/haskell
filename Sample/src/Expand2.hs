module Main where

tabStop = 8

main::IO()
main = getContents >>= putStr . expand

expand :: String -> String
expand = concatMap expandTab
-- pattern match
expandTab :: Char -> String
expandTab '\t' = replicate tabStop ' '
expandTab c    = [c]