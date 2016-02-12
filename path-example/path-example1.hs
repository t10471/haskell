{-# LANGUAGE TemplateHaskell #-}

import Path

main :: IO ()
main = do
  let ad = $(mkAbsDir "/home/chris")
  let rd = $(mkRelDir "chris")
  let af = $(mkAbsFile "/home/chris/x.txt")
  let rf = $(mkRelFile "chris/x.txt")

  putStrLn $ toFilePath $ ad </> rd
  putStrLn $ toFilePath $ parent ad
  putStrLn $ toFilePath $ filename af
  putStrLn $ toFilePath $ dirname ad
  r <- stripDir ad af
  putStrLn $ toFilePath r
  putStrLn "end"
