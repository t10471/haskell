{-# LANGUAGE TemplateHaskell #-}

import MyMacro
-- import qualified Web.Users.Types as U
import Web.Users.Types
-- lookupTableを動的に作成
precompute [1..1000]


main :: IO ()
main = do
  putStrLn $ show $ lookupTable 10
  putStrLn "end"
