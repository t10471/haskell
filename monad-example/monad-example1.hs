
import Example.Sheep
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import System.IO
import System.Environment
import System.Directory

exSequence :: IO ()
exSequence = do
  print $ sequence [Just 1, Just 2]
  -- Just [1,2]

exSequence' :: IO ()
exSequence' = do
  print $ sequence [Just 1, Nothing, Just 2]
  -- Nothing

exSequence_ :: IO ()
exSequence_ = do
  sequence_ [print "1",  print "2"]
  -- "1"
  -- "2"

fun :: Int -> Maybe Int
fun 0 = Nothing
fun x = Just x

exMapM :: IO ()
exMapM = do
  print $ mapM fun [1, 2]
  -- Just [1,2]

exMapM' :: IO ()
exMapM' = do
  print $ mapM fun [0, 1, 2]
  -- Nothing

exMapM_ :: IO ()
exMapM_ = do
  mapM_ print ["1", "2"]
  -- "1"
  -- "2"

exBind :: IO ()
exBind = return 1 >>= print
-- 1

exRevBind :: IO ()
exRevBind = print =<< return 1
-- 1

-- foldM のサンプル
-- traceFamily は先祖を見つけるためのジェネリック関数です
-- foldM :: (Sheep -> (Sheep -> Maybe Sheep) -> Maybe Sheep) -> Sheep -> [Sheep -> Maybe Sheep] -> Maybe Sheep
-- getParent :: t1 -> (t1 -> t) -> t
traceFamily :: Sheep -> [ (Sheep -> Maybe Sheep) ] -> Maybe Sheep
traceFamily s l = foldM getParent s l
  where getParent s f = f s

-- traceFamily を使って、複雑な問い合せを、簡単に、明瞭に、定義できます
mothersPaternalGrandfather s = traceFamily s [mother, father, father]
paternalGrandmother        s = traceFamily s [father, mother]

-- an Entry is a key and a value, both Strings
data Entry = Entry {
       key   :: String
     , value :: String
     }

-- show an entry as "key = value"
instance Show Entry where
  show e = show (key e) ++ " = " ++ (show (value e))

-- we parse "key = value" strings into Entry values
instance Read Entry where
  readsPrec _ s = readsEntry s

readsEntry :: ReadS Entry
readsEntry s = [(Entry (trim key) (trim val), s'') | (key, s')    <- [break (=='=') s],
                                                     (x:val, s'') <- [break (=='\n') s'] ]

-- remove leading and trailing whitespace
trim :: String -> String
trim s = dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

-- convenience function
openForReading :: FilePath -> IO Handle
openForReading f = openFile f ReadMode

-- Dict は文字列から文字列への単なる有限写像です
type Dict = Map.Map String String

-- この補助関数は foldl で使います
addEntry :: Dict -> Entry -> Dict
addEntry d e = Map.insert (key e) (value e) d 

-- この補助関数は IO モナドの中で foldM とともに使います
addDataFromFile :: Dict -> Handle -> IO Dict
addDataFromFile dict hdl = do 
    contents <- hGetContents hdl
    entries  <- return (map read (lines contents))
    return (foldl (addEntry) dict entries)

-- このプログラムは、コマンドラインから指定されたすべてのファイル中の
-- エントリから辞書を構築し、それを連想リストとして印字出力します
-- exDict ["dict1", "dict2"]
exDict :: [String] -> IO ()
exDict files = do 
    handles <- mapM  openForReading files
    dict    <- foldM addDataFromFile Map.empty handles
    print (Map.toList dict)
-- ここまで foldM

exFilterM ::[String] ->  IO ()
exFilterM names = do 
    dirs  <- filterM doesDirectoryExist names
    mapM_ putStrLn dirs

exZipWith :: IO ()
exZipWith = do 
    n  <- zipWithM (\x y -> return $ x + y) [1..5] [2..6] 
    print n

exZipWith_ :: IO ()
exZipWith_ = do 
    zipWithM_ (\x y -> return (x + y) >>= print) [1..5] [2..6] 

exCond :: Integer -> IO ()
exCond n = do
  when   (n == 1) $ print "eq  1"
  unless (n == 1) $ print "not 1"

-- listM の説明
-- listM はモナドをはがして( (<-) のこと) 処理をして
-- return するというのをラップしてくれる
swapNames :: String -> String
swapNames s = let (ln,fn) = break (==',') s
              in (dropWhile isSpace (tail fn)) ++ " " ++ ln

getName :: String -> Maybe String
getName name = do
    let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
    liftM swapNames (lookup name db)

{- これと同じ
getName name = 
    do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
    tempName <- lookup name db
    return (swapNames tempName)
-}
exLiftM :: String -> IO ()
exLiftM name = do
    case (getName name) of
      Just n  -> print n
      Nothing -> putStrLn "No such person in the database"
-- listM の説明 終わり

-- liftM2
-- allCombinations は与えられたリストの要素のすべての組合せた結果
-- 全体を二項演算子で畳み込んだ結果を含むリストを返します。
-- たとえば、allCombinations (+) [[0,1],[1,2,3]] は、
--   [0+1,0+2,0+3,1+1,1+2,1+3]、あるいは [1,2,3,2,3,4] となり、
-- また、allCombinations (*) [[0,1],[1,2],[3,5]] は、
--   [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5]、あるいは
-- [0,0,0,0,3,5,6,10] となります。
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations fn []     = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls

-- print an example
showExample :: (Show a) => String -> (a -> a -> a) -> [[a]] -> IO ()
showExample opName op ls = do 
  putStrLn $ "opName over " ++ (show ls) ++ " = "
  putStrLn $ "  " ++ (show (allCombinations op ls)) 

-- shows a few examples of using allCombinations
exLiftM2 :: IO ()
exLiftM2 = do 
  showExample "+" (+)   [[0,1],[1,2,3]]
  showExample "*" (*)   [[0,1],[1,2],[3,5]]
  showExample "/" div   [[100, 45, 365], [3, 5], [2, 4], [2]]
-- liftM2 終わり

{- ap は $ のモナド版
ap :: (Monad m) => m (a -> b ) -> m a -> m b
ap = liftM2 ($)
-}
exAp :: IO ()
exAp = do
  print $ [(*2),(+3)] `ap` [0,1,2]
  print $ (Just (*2)) `ap` (Just 3)

exAp' :: Integer -> String -> IO ()
exAp' i j = do 
    let fns  = [("double", (2*))
              , ("halve" , (`div`2))
              , ("square", (\x->x*x))
              , ("negate", negate)
              , ("incr"  , (+1))
              , ("decr"  , (+(-1)))
               ]
        cmds = map ((flip lookup) fns) (words j)
    print $ foldl (flip ap) (Just i) cmds

-- msum
type Variable = String
type Value = String
type EnvironmentStack = [[(Variable,Value)]]

-- lookupVar は環境スタックから変数の値を検索します。
-- これは msum を Maybe モナド内で使い、最初の非-Nothing 値を返します
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var stack = msum $ map (lookup var) stack

{- 下記と同じ
lookupVar :: Variable -> EnvironmentStack -> Maybe Value
lookupVar var []     = Nothing
lookupVar var (e:es) = let val = lookup var e
                       in maybe (lookupVar var es) Just val
--}

{-
exMsum "depth" [[("name","test"),("depth","2")], [("depth","1")]]
exMsum "width" [[("name","test"),("depth","2")], [("depth","1")]]
exMsum "var3"  [[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]
exMsum "var2"  [[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]
exMsum "var1"  [[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]
exMsum "var"   [[("var1","value1"),("var2","value2*")], [("var2","value2"),("var3","value3")]]
-}
exMsum :: String -> EnvironmentStack -> IO ()
exMsum k s = do 
    print $ lookupVar k s
-- msum 終わり

-- guard
{-
guard :: MonadPlus m => Bool -> m ()
guard p = if p then return () else mzero
-}
--  mzero :: [Int]     => []
--  mzero :: Maybe Int => Nothing
--
-- we have records containing name and age
data Record = Rec {name::String, age::Int} deriving Show
-- we collect the records into a "database"
type DB = [Record]
-- getYoungerThan は指定した年齢より若い人の、全てのレコードを返します。
-- これはガード関数を用いて、制限値およびそれを越える年齢の人のレコード
-- を消去するのに使います。これは例示にすぎません。実際には単に filter
-- を使う方が明解です。filter の許容値がもっと複雑な場合には、guard が
-- もっと役に立つでしょう。
getYoungerThan :: Int -> DB -> [Record]
getYoungerThan limit db =
  flip mapMaybe db $ \r -> do 
    guard (age r < limit) 
    -- Trueのときだけここに来る
    return r

{-
exGuard 30
-}
exGuard :: Int -> IO ()
exGuard k = do 
    let db = [Rec "Marge" 37, Rec "Homer" 38, Rec "Bart" 11, Rec "Lisa" 8, Rec "Maggie" 2]
    print $ getYoungerThan k db


main :: IO ()
main = do
  putStrLn "end"
