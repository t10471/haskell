module Main where

tabStop = 8

main::IO()
main = do cs <- getContents
          putStr $ expand cs

expand :: String -> String
expand cs = concat $ map expandTab cs
-- pattern match
expandTab :: Char -> String
expandTab '\t' = replicate tabStop ' '
expandTab c    = [c]