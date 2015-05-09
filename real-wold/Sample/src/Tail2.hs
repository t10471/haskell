module Main where

main::IO()
main = do cs <- getContents
          putStr $ lastNLines 10 cs

lastNLines n = unlines . takeLast n . lines

takeLast n = reverse . take n . reverse