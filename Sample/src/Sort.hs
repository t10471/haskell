module Main where
import List
main::IO()
main = putStr . unlines .sort .lines =<< getContents
