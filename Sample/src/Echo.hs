module Main where

import System

main::IO()
main = do args <- getArgs
          putStrLn $ unwords args
