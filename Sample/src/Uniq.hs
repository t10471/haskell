module Main where
import List
main::IO()
-- group "aabc" ["aa","b","c"]
-- group ["aa","aa","bb", "bb"] [["aa","aa"],["bb","bb"]]
main = putStr . unlines .map (head) . group .lines =<< getContents