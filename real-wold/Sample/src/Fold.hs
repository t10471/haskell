module Main where

foldWidth = 60

main::IO()
main = do cs <- getContents
          print $ fold cs

fold cs = unlines $ concatMap foldLine $ lines cs

foldLine line = case splitAt foldWidth line of
                (s, [])    -> [s]
                (s, count) -> s : foldLine count