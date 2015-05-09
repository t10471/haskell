module Main where

main::IO()
main = do cs <- getContents
          print $ length cs

