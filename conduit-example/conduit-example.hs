{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit (
                    ($$) 
                  , ($=)
                  , (=$)
                  , (=$=)
                  , ($$+)
                  , ($$++)
                  , ($$+-)
                  , Source
                  , Conduit
                  , Sink
                  , yield
                  , yieldOr
                  , await
                  , awaitForever
                  , leftover 
                  , addCleanup
                  , bracketP
                  )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Monoid ((<>))
import Data.Word (Word8)
import Data.Char (ord, chr, toUpper, toLower)
import qualified Data.ByteString as BS
import Control.Monad (when)
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State
import System.IO (
                  stdin
                , stdout
                , openFile
                , hGetChar
                , hClose
                , hIsEOF
                , IOMode(..)
                )
import Debug.Trace

-- シンプルな使い方
exSimple :: IO ()
exSimple = do
  source $$  conduitScd =$  conduitFst  =$ sink
  source $=  conduitScd  $$ conduitFst  =$ sink
  source $=  conduitScd  $= conduitFst  $$ sink
  source $= (conduitScd =$= conduitFst) $$ sink
  source $$ (conduitScd =$= conduitFst) =$ sink

source :: Source IO Int
source = CL.sourceList [1..4]

-- sourceList :: Monad m => [a] -> Source m a
-- sourceList = mapM_ yield

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

-- awaitForever を使った version
-- sink :: Sink String IO ()
-- sink = awaitForever $ liftIO . putStrLn

-- awaitForever を自分で書いた version
myAwaitForever :: Monad m => (a -> Conduit a m b ) -> Conduit a m b
myAwaitForever f = 
      await >>= maybe (return ()) (\x -> f x >> myAwaitForever f)

conduitFst :: Conduit Int IO String
conduitFst = CL.map show

conduitScd :: Conduit Int IO Int
conduitScd = CL.map (* 2 )

-- yield await leftover 使ったサンプル
exSimple' :: IO ()
exSimple' = do
  source' $= conduit' $$ sink'

source' :: Source IO Int
source' = do
    yield 1
    yield 2
    yield 3
    yield 4

sink' :: Sink String IO ()
sink' = do
    s <- await
    case s of
        Nothing -> return ()
        Just s' -> do
            liftIO $ putStrLn s'
            sink'

conduit' :: Conduit Int IO String
conduit' = do
    i <- await
    j <- await
    case (i, j) of
        (Just i', Just j') -> do
            yield $ show (i', j')
            -- leftover :: i -> ConduitM i o m ()
            leftover j'
            conduit'
        _ -> return ()

-- 入力と同じ値を3つ返すサンプル
exTriple :: IO ()
exTriple = CL.sourceList [1..4] $$ triple =$ CL.mapM_ print

triple :: Monad m => Conduit a m a
triple = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> do
            CL.sourceList [a, a, a]
            triple

-- 最初の値をそれ以降にかける
-- multiplier は一度しか通らない
exMultiplier :: IO ()
exMultiplier = CL.sourceList [5..10] $$ multiplier =$ CL.mapM_ print

multiplier :: Monad m => Conduit Int m Int
multiplier = do
    ma <- await
    case ma of
        Nothing -> return ()
        Just a -> CL.map (* (trace ("a is " ++ show a) a))

--State モナドを使った lift の例 
exStateLift :: IO ()
exStateLift = do
    let result :: State Int [(Int, Int)]
        result = source'' $$ conduit'' =$ CL.consume
    print $ runState result 5

source'' :: Source (State Int) Int
source'' = do
    x <- lift get
    if x <= 0
        then return ()
        else do
            yield x
            lift $ modify (\x -> x - 2)
            source''
            
conduit'' :: Conduit Int (State Int) (Int, Int)
conduit'' = awaitForever $ \i -> do
    lift $ modify (+ 1)
    x <- lift get
    yield (i, x)

-- sink -> conduit -> source -> conduit -> sink の順にログがでる
-- 引数が 2 のときは全てデータがあるが
-- 引数が 4 のときは途中でデータが無くなって終わる
exSlink :: IO ()
exSlink = source''' $$ conduit''' =$ sink''' 2
-- exSlink = source''' $$ conduit''' =$ sink''' 4

source''' :: Source IO Int
source''' = do
    liftIO $ putStrLn "source: yielding 1"
    yield 1
    liftIO $ putStrLn "source: yielding 2"
    yield 2
    liftIO $ putStrLn "source: yielding 3"
    yield 3
    
conduit''' :: Conduit Int IO Int 
conduit''' = do
    liftIO $ putStrLn "conduit calling await"
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "Nothing left, exiting"
        Just x -> do
            liftIO $ putStrLn $ "conduit yielding " ++ show x
            yield x
            conduit'''
            
sink''' ::  Int -> Sink Int IO ()
sink''' 0 = liftIO $ putStrLn "sink is finished, terminating"
sink''' i = do
    liftIO $ putStrLn $ "sink: still waiting for " ++ show i
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
        Just x -> do
            liftIO $ putStrLn $ "sink received: " ++ show x
            sink''' (i - 1)
            

-- 7 で止める
exCleanUp :: IO ()
exCleanUp = source'''' $$ CL.isolate 7 =$ CL.mapM_ print

source'''' =
    loop 1
  where
    loop i = do
        -- yield i
        -- yieldOr で最後が分かる
        yieldOr i $ putStrLn $ "Terminated when yielding: " ++ show i
        loop $ i + 1
        

-- addcleanup で終了処理を定義
exAddCleanUp :: IO ()
exAddCleanUp = source''''' $$ CL.isolate 5 =$ CL.mapM_ print

source''''' = do
    handle <- liftIO $ openFile "test.txt" ReadMode
    addCleanup (const $ putStrLn "Closing handle" >> hClose handle) $ loop handle
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle
                

-- 上の例は例外安全ではないので
-- bracket のように使える bracketP
exException :: IO ()
exException = runResourceT $ source'''''' $$ exceptionalSink

source'''''' =
    bracketP
        (openFile "test.txt" ReadMode)
        (\handle -> putStrLn "Closing handle" >> hClose handle)
        loop
  where
    loop handle = do
        eof <- liftIO $ hIsEOF handle
        if eof
            then return ()
            else do
                c <- liftIO $ hGetChar handle
                yield c
                loop handle

exceptionalSink = do
    c <- await
    liftIO $ print c
    error "This throws an exception"

exResumableSource' :: IO ()
exResumableSource' = do
    (rsrc1, result1) <- CL.sourceList [1..10] $$+ CL.take 3
    (rsrc2, result2) <- rsrc1 $$++ CL.take 3
    result3 <- rsrc2 $$+- CL.consume
    print (result1, result2, result3)

-- List サンプル
exList :: IO ()
exList = do
  -- consume    :: Monad m => Consumer a m [a]
  -- sourceList :: Monad m => [a] -> Producer m a
  xs <- CL.sourceList ['a'..'z'] $$ CL.consume
  putStrLn xs

-- Source の 結合
exJoin :: IO ()
exJoin = do
  xs <- (srcA <> srcB) $$ CL.consume -- ()は見やすさの為
  print xs

srcA :: Monad m => Source m Int
srcA = CL.sourceList [1..10]

srcB :: Monad m => Source m Int
srcB = CL.sourceList [11..20]

exSource :: IO ()
exSource = mySrc $$ CL.mapM_ putStrLn

mySrc :: (Monad m, MonadIO m) => Source m String
mySrc = do
    x <- liftIO $ getLine
    if x == "END"
        then return ()
        -- yield :: Monad m => o -> ConduitM i o m ()
        else yield x >> mySrc

exResumableSource :: IO ()
exResumableSource = do
    (resumableSrc0, xs) <- CL.sourceList [1..25] $$+ CL.take 10
    display xs
    (resumableSrc1, ys) <- resumableSrc0 $$++ CL.take 10
    display ys
    zs <- resumableSrc1 $$+- CL.take 10
    display zs

display :: [Int] -> IO ()
display xs = do
    print xs
    putStrLn "-----"

exMapM :: IO ()
exMapM = CL.sourceList ['a'..'z'] $$ CL.mapM_ print

exSink :: IO ()
exSink = (CL.sourceList [1..30] $$ mySink) >>= print

-- | 入力を標準出力へ出力し、受け取った値の総数を返します。3の倍数でアホになります。
mySink :: (Monad m, MonadIO m) => Sink Int m Int
mySink = go 0
    where
        go l = do
            -- await :: Monad m => Consumer i m (Maybe i)
            n <- await
            case n of
                Nothing -> return l
                Just n'   -> do
                    if n' `mod` 3 == 0
                        then liftIO $ putStrLn "(^q^)"
                        else liftIO $ print n'
                    go $ l + 1

exConduit :: IO ()
exConduit = do
    CL.sourceList ['a'..'z'] $= CL.map toUpper $$ CL.mapM_ print
    CL.sourceList ['A'..'Z'] $$ CL.map toLower =$ CL.mapM_ print
    CL.sourceList ['a'..'z'] $= CL.map toUpper =$= CL.map fromEnum $$ CL.mapM_ print

exMyConduit :: IO ()
exMyConduit = CL.sourceList (['a'..'s'] ++ ['a'..'d']) $= ascend $= tupleThree $$ CL.mapM_ print

-- | 入力を3つ毎に区切り、そのタプルを送出します
--   取得できた入力が3つ未満の場合は切り捨てます。
tupleThree :: Monad m => Conduit a m (a, a, a)
tupleThree = do
    xs <- CL.take 3
    case xs of
        [a,b,c] -> yield (a,b,c) >> tupleThree
        _       -> return ()

-- | 入力の値が昇順になっている間だけ値を送出します。
ascend :: (Monad m, Ord i) => Conduit i m i
ascend = await >>= maybe (return ()) go
    where
        go i = do
            yield i
            await >>= maybe (return ()) (\j -> when (i <= j) $ go j)

exFilterUc :: IO ()
exFilterUc = runResourceT $ do
  CB.sourceHandle stdin
    $= CB.takeWhile (\c -> c /= word8 '.')
    $= filterUc
    $$ CB.sinkHandle stdout
  return ()
  where
    word8 = fromIntegral . ord


filterUc :: Monad m => Conduit BS.ByteString m BS.ByteString
filterUc = CL.map ucString
  where
    uc :: Word8 -> Word8
    uc = fromIntegral . ord . toUpper . chr . fromIntegral
    ucString :: BS.ByteString -> BS.ByteString
    ucString = BS.map uc

exTakeWhileStr = do
  ss <-
    runResourceT $
    CB.sourceFile "hogehoge"
    $= CB.lines
    $= takeWhile' (/= "END")
    $$ CL.consume
  print ss

-- Data.Conduit.Binary に takewhile があるが Word8 なので
-- これは String
takeWhile' :: Monad m => (a -> Bool) -> Conduit a m a
takeWhile' f = do
  mx <- await
  case mx of
    Nothing -> return ()
    Just x
      | f x -> yield x >> takeWhile' f
      | otherwise -> return ()

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runResourceT $ CB.sourceFile src $$ CB.sinkFile dest

main :: IO ()
main = do
  exSimple
  exSimple'
  exTriple
  exMultiplier
  -- exList
  -- exJoin
  -- exSource
  -- exResumableSource
  -- exMapM
  -- exSink
  -- exConduit
  -- exMyConduit
  -- exFilterUc
  -- exTakeWhileStr
  putStrLn "end"
