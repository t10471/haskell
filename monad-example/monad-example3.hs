
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import qualified Data.Map.Strict as Map
import Data.Char

-- Reader モナドと Writer モナド Cont モナドのサンプル

-- Reader モナドはデータとデータを使用する関数から成り立つ
type Dict = Map.Map Int String
type DictR = Reader Dict Int

dictD :: Dict
dictD = Map.fromList [(0, "abc"), (1, "mnl"), (2, "xyz")]

addDict :: Dict -> Dict
addDict = Map.insert 3 "stu" 

sizeD :: Dict -> Int
sizeD = Map.size
-- Reader モナドの作成
readerR :: DictR
readerR = reader sizeD
-- ask データを取得
askR :: DictR
askR = do
  r <- ask
  return $ sizeD r
-- asks 取得下データを加工した結果を返す
asksR :: DictR
asksR = do
  r <- asks $ (+1) . sizeD  
  return r
-- local 一時的にデータ自体を変更できる
-- runReader みたいなもの
localR :: DictR
localR = do
  r <- local (Map.insert 3 "opq") $ do
    r' <- ask
    return $ sizeD r'
  return r

-- local をつかったループ
localR' :: DictR
localR' = do
    r <- asks $ sizeD
    loop r
  where
    loop r
      | r > 5     = return r
      | otherwise = local (Map.insert r $ show r) localR'

-- Writer モナド
-- データとデータを使用した関数の結果から成り立つ
type ListW = Writer [String] Int 

mergeD :: Dict -> String
mergeD =  Map.foldr (++) ""

addS :: [String] -> [String]
addS []  = ["empty"]
addS [s] = ["add " ++ s]

convertS :: [String] -> [String]
convertS []         = []
convertS ["writer"] = ["convert writer"]
convertS ["tell"]   = ["convert tell"]
convertS ["listen"] = ["convert listen"]
convertS ["inner"]  = ["convert inner"]
convertS s          = s
-- Writer モナドの作成
writerW :: ListW
writerW = writer (0, ["writer"])
-- tell データを追加する
tellW :: ListW
tellW = do
  tell ["tell"]
  return 0 
-- listen 他のWriterモナドから
-- データを結果を貰う
listenW :: ListW
listenW = do
  tell ["listen"]
  (d, s) <- listen writerW
  tell s
  return $ d + 1
-- listen を使ったループ
listenW' :: ListW -> ListW
listenW' w = do
    (d, s) <- listen w
    loop d
  where
    loop d
      | d > 5     = return d
      | otherwise = listenW' $ writer (d+1, [show $ d+1])

-- listens 他のWriterモナドから
-- データを結果を貰う
-- ただし、加工してからもらう
listensW :: ListW
listensW = do
  tell ["listens"]
  (d, s) <- listens convertS writerW
  tell s
  return $ d + 1
-- pass 結果のコンバート処理などを行う時に
-- 使用する
-- pass 内で tell をするとうまく動かない
-- listten しないと空になる
passW :: ListW -> ListW
passW w = pass $ do 
  (d, s) <- listen w
  return (d, convertS)
 -- censor pass を使いやすくしたもの 
censorW :: ListW -> ListW
censorW = censor convertS

type DictC = Cont Int Dict
type ListD = [(Int, String)]
type ListC = Cont Int ListD

dictF :: (Dict -> Int) -> Int
dictF f = f dictD

contC :: DictC
contC = cont dictF

convDL :: (ListD -> Int) -> Dict -> Int
convDL f d = f $ Map.toList d

-- Cont モナド
-- continuation-example に詳しいのがある
exCont :: Int -> Cont String String
exCont n = do
        str <- callCC $ \exit1 -> do
          when (n < 10) (exit1 (show n))
          let ns = map digitToInt (show (n `div` 2))
          n' <- callCC $ \exit2 -> do 
            when ((length ns) < 3) (exit2 (length ns))
            when ((length ns) < 5) (exit2 n)
            when ((length ns) < 7) $ do 
              let ns' = map intToDigit (reverse ns)
              exit1 (dropWhile (=='0') ns')
            return $ sum ns
          return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
        return $ "Answer: " ++ str

type IString = IO String

-- Const の ConstTの導入のための
-- ConstTを使わないモナドの合成
toIO :: a -> IO a
toIO x = return x

exContIO :: Int -> Cont IString IString
exContIO n = do
    str <- callCC $ \exit1 -> do
      when (n < 10) (exit1 $ toIO (show n))
      let ns = map digitToInt (show (n `div` 2))
      n' <- callCC $ \exit2 -> do
        when ((length ns) < 3) $ exit2 $ do 
          let ll =  length ns
          print "length ns < 3"
          return ll
        when ((length ns) < 5) $ exit2 $ do 
          print "length ns < 5"
          return n
        when ((length ns) < 7) $ do 
          let ns' = map intToDigit (reverse ns)
          exit1 $ toIO (dropWhile (=='0') ns')
        return (toIO (sum ns))
      return $ do 
        num <- n'
        return $ "(ns = " ++ (show ns) ++ ") " ++ (show num)
    return $ do 
        s <- str
        return $ "Answer: " ++ s

-- ConstT を使ったモナドの合成
exContT :: Int -> ContT String IO String
exContT n = do
    str <- callCC $ \exit1 -> do
      when (n < 10) (exit1 (show n))
      let ns = map digitToInt (show (n `div` 2))
      n' <- callCC $ \exit2 -> do
        when ((length ns) < 3) $ do
          liftIO $ print "length ns < 3"
          exit2 $ length ns
        when ((length ns) < 5) $ do
          liftIO $ print "length ns < 5"
          exit2 n
        when ((length ns) < 7) $ do 
          liftIO $ print "length ns < 7"
          let ns' = map intToDigit (reverse ns)
          exit1 $ dropWhile (=='0') ns'
        liftIO $ print "other"
        return $ sum ns
      return $ "(ns = " ++ (show ns) ++ ") " ++ (show n')
    return str

main :: IO ()
main = do
  print $ runReader readerR dictD 
  print $ runReader askR dictD
  print $ runReader asksR dictD
  print $ runReader localR dictD
  print $ runReader localR' dictD
  -- 意味はないけど処理を連結
  -- >>= を使う場合は \_ -> askR と関数にする
  print $ flip runReader dictD $ 
    readerR >> askR >> asksR >> localR
  -- withReader を使って環境を上書きできる
  print $ runReader (withReader addDict readerR) dictD
  -- 上と同じ
  print $ (runReader readerR . addDict) dictD

  print $ runWriter writerW
  print $ runWriter tellW
  print $ runWriter listenW 
  print $ runWriter $ listenW' writerW
  print $ runWriter listensW 
  print $ runWriter $ passW writerW
  print $ runWriter $ censorW writerW

  print $ runCont (exCont 20) id
  print $ runCont contC Map.size
  -- withCont を使って データの型を変換できる
  print $ runCont (withCont convDL contC) length 
  -- 上と同じ
  print $ (runCont contC . convDL) length 
  print $ runCont (exCont 10) id
  r <- runCont (exContIO 10) id
  print r
  r <- runContT (exContT 10) return
  print r
  putStrLn "end"


