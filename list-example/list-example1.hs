
import Control.Monad.Trans
import Data.List
import Data.Maybe

exListA :: IO ()
exListA = do
  print $ 1 : [2, 3]
  print $ [1,2,3] ++ [4,5,6]
  print $ map (^2) [1,2,3]
  print $ sort [2,1,3]
  print $ head [1,2,3,4]
  -- 1
  print $ tail [1,2,3,4]
  -- [2,3,4]
  print $ last [1,2,3,4]
  -- 4
  print $ init [1,2,3,4]
  -- [1,2,3]
  print $ length [1,2,3,4]
  print $ [1,2,3,4] !! 0
  -- 1
  print $ splitAt 2 [1,2,3,4]
  -- ([1,2],[3,4])
  print $ null []
  -- True
  -- null を使った再帰
  let mysum (x:xs) = if (null xs) then x else x + mysum xs
  print $ mysum [1..10]

exListB :: IO ()
exListB = do
  print $ reverse [1,2,3,4]
  print $ sortBy (\x y -> compare y x) [2,3,8,5]
  let vocaloids = [("Miku",16), ("Rin", 14), ("Len", 14), ("Luka", 20)]
  print $ sortBy (\x y -> compare (snd x) (snd y)) vocaloids
  print $ (read "[1,2,3,4]" :: [Int])
  print $ lines "hello\nworld"
  print $ unlines ["hello", "world"]
  print $ words "hello world"
  print $ unwords ["hello", "world"]
  print $ intersperse 0 [1,2,3]
  -- [1,0,2,0,3]
  print $ concat $ intersperse "," ["hello", "world"]
  -- "hello,world"
  print $ intercalate "," ["hello", "world"]
  -- "hello,world"
  print $ elem 2 [1,2,3,4]
  -- True
  print $ filter even [1..10]
  print $ find (>3) [0,2,4,6,8]
  -- Just 4
  print $ fromJust $ find (>3) [0,2,4,6,8]
  -- 4
  print $ lookup 'c' [('a',0), ('b',1), ('c', 2)]
  -- Just 2
  let logic = [("Socratese", "human"), ("human", "mortal")]
  print $ lookup "Socratese" logic >>= (flip lookup) logic
  -- Just "mortal"

exListBA :: IO ()
exListBA = do
  print $ takeWhile even [2,2,1,2,2]
  -- [2,2]
  print $ dropWhile even [2,2,1,2,2]
  -- [1,2,2]
  print $ span even [2,2,1,2,2]
  -- ([2,2],[1,2,2])
  print $ break odd [2,2,1,2,2]
  -- ([2,2],[1,2,2])

exListC :: IO ()
exListC = do
  print $ zip [1,2,3] [4,5,6]
  print $ zipWith (+) [1,2,3] [4,5,6]
  print $ unzip [(1,4), (2,5), (3,6)]
  print $ zip3 [1,2,3] [4,5,6] [7,8,9]
  print $ zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,6] [7,8,9]
  print $ unzip3 [(1,4,7),(2,5,8),(3,6,9)]

exListD :: IO ()
exListD = do
  -- 重複を削除
  print $ nub [0,1,2,3,2,1,0]
  -- [0,1,2,3]
  print $ nub "AAAAAAAAAABBBBBBCCCC"
  -- "ABC"
  -- 最初の数字は消えないことに注意
  print $ nubBy (\x y -> x+y == 10) [2,3,5,7,8]
  -- [2,3,5]
  -- 並び順によって結果が変わる
  print $ nubBy (\x y -> x+y == 10) [8,7,5,3,2]
  -- [8,7,5]
  -- 複数あっても最初のみ削除される
  print $ delete 2 [1,2,3,2,1]
  -- [1,3,2,1]
  print $ deleteBy (\x y -> y `mod` x == 0) 4 [6,8,10,12]
  -- [6,10,12]
  -- 間に挿入
  print $ insert 4 [1,3,5,7,9]
  -- [1,3,4,5,7,9]
  -- 先頭に挿入
  print $ insert 4 [4,5,3,6,2]
  -- [4,4,5,3,6,2]
  let xxx a b = if (a+b < a*b) then LT else GT
  print $ insertBy xxx 4 [0,1,3,5,7,9]
  -- [0,1,4,3,5,7,9]

exListE :: IO ()
exListE = do
  print $ foldl (-) 0 [1,2,3,4,5]
  print $ foldr (-) 0 [1,2,3,4,5]
  let a = [1..10]
  print $ (sum a) / fromIntegral (length a)
  print $ foldl (flip (:)) [] [1,2,3,4,5]
  -- [5,4,3,2,1]
  print $ concat [[1,2],[3],[4,5]]
  print $ foldr1 (++) [[1,2],[3],[4,5]]
  print $ concatMap tail [[1,2,3],[4,5,6]]
  -- [2,3,5,6]
  print $ foldr1 (++) $ map tail [[1,2,3],[4,5,6]]
  -- [2,3,5,6]
  print $ and [True, False, True]
  print $ foldr1 (&&) [True, False, True]
  print $ or [True, False, True]
  print $ foldr1 (||) [True, False, True]
  print $ any (>3) [2,3,4]
  print $ foldr1 (||) $ map (>3) [2,3,4]
  print $ all (>3) [2,3,4]
  print $ foldr1 (&&) $ map (>3) [2,3,4]
  print $ sum [1..10]
  print $ foldr1 (+) [1..10]
  print $ product [1..5]
  print $ foldr1 (*) [1..5]
  print $ maximum [2,9,7,3]
  print $ foldr1 max [2,9,7,3]
  print $ minimum [2,9,7,3]
  print $ foldr1 min [2,9,7,3]

exListF :: IO ()
exListF = do
  print $ scanl (+) 0 [1..5]
  -- [0,1,3,6,10,15]
  print $ scanr (+) 0 [1..5]
  -- [15,14,12,9,5,0]
  print $ scanl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..5])
  -- ["0","(0+1)","((0+1)+2)","(((0+1)+2)+3)","((((0+1)+2)+3)+4)","(((((0+1)+2)+3)+4)+5)"]
  print $ scanr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..5])
  -- ["(1+(2+(3+(4+(5+0)))))","(2+(3+(4+(5+0))))","(3+(4+(5+0)))","(4+(5+0))","(5+0)","0"]
  print $ last $ scanl (-) 0 [1..5]
  print $ foldl (-) 0 [1..5]
  -- -15
  print $ head $ scanr (-) 0 [1..5]
  print $ foldr (-) 0 [1..5]
  -- 3
  print $ scanl1 (+) [1..5]
  -- [1,3,6,10,15]
  print $ scanl1 (\x y -> concat ["(",x,"+",y,")"]) (map show [1..5])
  -- ["1","(1+2)","((1+2)+3)","(((1+2)+3)+4)","((((1+2)+3)+4)+5)"]
  print $ scanr1 (+) [1..5]
  -- [15,14,12,9,5]
  print $ scanr1 (\x y -> concat ["(",x,"+",y,")"]) (map show [1..5])
  -- ["(1+(2+(3+(4+5))))","(2+(3+(4+5)))","(3+(4+5))","(4+5)","5"]

exListFA :: IO ()
exListFA = do
  print $ mapAccumL (\x y -> (x,x*y)) 5 [9,6,3]
  -- (5,[45,30,15])
  print $ mapAccumL (\x y -> (x, concat [x,"*",y])) "5" ["9","6","3"]
  -- ("5",["5*9","5*6","5*3"])
  print $ mapAccumL (\x y -> (x+y,x*y)) 5 [2,4,8]
  -- (19,[10,28,88])
  print $ mapAccumL (\x y -> (concat ["(",x,"+",y,")"], concat [x,"*",y])) "5" ["2","4","8"]
  -- ("(((5+2)+4)+8)",["5*2","(5+2)*4","((5+2)+4)*8"])
  print $ mapAccumL (\x y -> (x+y,y)) 5 [2,4,8]
  -- (19,[2,4,8])
  print $ mapAccumL (\x y -> (concat ["(",x,"+",y,")"], y)) "5" ["2","4","8"]
  -- ("(((5+2)+4)+8)",["2","4","8"])
  print $ mapAccumL (\x y -> (y,y)) 5 [2,4,8]
  -- (8,[2,4,8])
  print $ mapAccumL (\x y -> (x,x)) 5 [2,4,8]
  -- ("5",["5","5","5"])
  print $ mapAccumR (\x y -> (x+y,x*y)) 5 [2,4,8]
  -- (19,[34,52,40])
  print $ mapAccumR (\x y -> (concat ["(",x,"+",y,")"], concat [x,"*",y])) "5" ["2","4","8"]
-- ("(((5+8)+4)+2)",["((5+8)+4)*2","(5+8)*4","5*8"])
  print $ zipWith (*) (tail (scanr (+) 5 [2,4,8])) [2,4,8]
  -- [34,52,40]
  print $ mapAccumL (\x y -> (x+y,x*y)) 5 [2,4,8]
  -- (19,[10,28,88])
  print $ zipWith (*) (init (scanl (+) 5 [2,4,8])) [2,4,8]
  -- [10,28,88]

exListG :: IO ()
exListG = do
  print $ take 5 $ cycle [1,2,3]
  -- [1,2,3,1,2]
  print $ take 5 $ repeat 2
  -- [2,2,2,2,2]
  print $ replicate 5 2
  -- [2,2,2,2,2]
  print $ zip [1..5] (iterate (*2) 2)
  -- [(1,2),(2,4),(3,8),(4,16),(5,32)]
  print $ take 5 $ iterate (*2) 1
  -- [1,2,4,8,16]
  -- foldr が関数とリストを引数にして畳み込みの結果を戻すのに対し、foldr
  -- は畳み込みの結果と foldr の逆関数を引数にして、元のリストを復元する。
  -- ここでは iterate と同様の結果を返すサンプルを記述
  print $ take 5 $ unfoldr (\x -> Just (x, (*2) x)) 1
  -- [1,2,4,8,16]
  print $ take 5 $ unfoldr (\x -> Just (x, (\y -> concat ["(",y,"*2)"]) x)) "1"
  -- ["1","(1*2)","((1*2)*2)","(((1*2)*2)*2)","((((1*2)*2)*2)*2)"]
  print $ (unfoldr (\x -> Nothing) 1 :: [Int])
  -- []
  print $ unfoldr (\x -> if x > 5 then Nothing else Just(x, x * 2)) 1
  -- [1,2,4]

exListGA :: IO ()
exListGA = do
  print $ isPrefixOf "I" "I really like Haskell."
  print $ isSuffixOf "Haskell." "I really like Haskell."
  print $ isInfixOf "really" "I really like Haskell."
  print $ isInfixOf [3,4] [1..10]
  print $ [1..10] !! 5
  print $ elemIndex 4 [1..10]
  print $ elemIndex 11 [1..10]
  let jDouble (Just x) = x + x
  print $ jDouble (Just 2)
  print $ fromJust (Just 2) * 2
  -- 第１引数と一致するリスト内の全ての要素のインデックスのリストを返す
  print $ elemIndices 2 [1,2,3,1,2,3]
  -- 条件に合うものの最初のインデックスを返す
  print $ findIndex (>3) [1..5]
  -- 条件に合うものの全てのインデックスを返す
  print $ findIndices (>3) [1..5]

exListH :: IO ()
exListH = do
  print $ transpose [[1,2,3],[4,5,6]]
  -- [[1,4],[2,5],[3,6]]
  print $ subsequences "abc"
  -- ["","a","b","ab","c","ac","bc","abc"]
  print $ filter (\x -> length x == 3) $ subsequences "abcde"
  -- ["abc","abd","acd","bcd","abe","ace","bce","ade","bde","cde"]
  print $ permutations "abc"
  -- ["abc","bac","cba","bca","cab","acb"]
  print $ nub $ map (take 2) $ permutations "abcd"
  -- ["ab","ba","cb","bc","ca","ac","dc","cd","db","bd","da","ad"]
  print $ group "Mississippi"
  -- ["M","i","ss","i","ss","i","pp","i"]
  print $ inits "abc"
  -- ["","a","ab","abc"]
  print $ tails "abc"
  -- ["abc","bc","c",""]



data PacketL a = WrapL a
    deriving (Show, Eq)

instance Monad PacketL where
    return x = WrapL x
    (WrapL x) >>= f = f x

main :: IO ()
main = do
  putStrLn "end"
