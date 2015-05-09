{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit 
import qualified Data.Conduit.Combinators as CC
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char
import Data.Text
import Control.Monad
import System.IO

-- ミーリ・マシンの実装
-- 出力が現在状態と入力によって決定される有限オートマトン

-- 基本型
-- event が入力 signal が出力
data Machine state event signal = Machine
    { mCurState :: state
    , mTransFunction :: state -> event -> (state, signal)
    }
-- 次の状態に以降させる
stepMachine :: Machine state event signal -> event -> (Machine state event signal, signal)
stepMachine machine event = (machine {mCurState = newState}, output)
    where
        curState = mCurState machine
        (newState, output) = mTransFunction machine curState event
-- 初期状態と状態変更関数を引数に鶴
createMachine :: state -> (state -> event -> (state, signal)) -> Machine state event signal
createMachine = Machine

-- 計算の出力
data CalcSignal = CalcNothing | CalcResult Int | CalcError CalcStates String
    deriving (Eq, Show)
-- 計算の状態
data CalcStates = Start | ReadFirst Int | ReadSecond Int Int | ReadOperator Int Int
    deriving (Eq, Show)
-- 計算機
type CalcMachine = Machine CalcStates Char CalcSignal
-- 計算機の実装
calcMachine :: CalcMachine
calcMachine = createMachine Start go
    where
        go Start e
            | isNumber e = (ReadFirst (read [e]), CalcNothing)
            | otherwise = (Start, CalcError Start "No number")

        go s@(ReadFirst i) e
            | isNumber e = (ReadFirst (10 * i + read [e]), CalcNothing)
            | isSpace e = (ReadSecond i 0, CalcNothing)
            | otherwise = (Start, CalcError s "Bad format")

        go s@(ReadSecond l i) e
            | isNumber e = (ReadSecond l (10 * i + read [e]), CalcNothing)
            | isSpace e = (ReadOperator l i, CalcNothing)
            | otherwise = (Start, CalcError s "Bad format")

        go s@(ReadOperator i j) e
            | e == '+' = (Start, CalcResult (i + j))
            | isSpace e = (s, CalcNothing)
            | otherwise = (Start, CalcError s "Bad operator")

-- calculate "56 67 +"
-- で動作確認できる
calculate :: String -> IO ()
calculate = foldM_ go calcMachine
    where 
        -- c はイベント
        go mach c = do
            let (m, s) = stepMachine mach c
            print s 
            return m

-- conduit を使用
process :: ResourceT IO ()
process = source $= calc $$ output
    where
        source = CC.stdin
        output = CC.stdout
        calc = CC.concatMap unpack =$= wrapMachine calcMachine =$= CC.filter f =$= CC.map (pack . show)
            where
                f (CalcResult _) = True
                f _              = False

wrapMachine :: (Monad m) => Machine state event signal -> Conduit event m signal
wrapMachine mach = do
    maybeEvent <- await
    case maybeEvent of
        Nothing -> return ()
        Just event -> let
                (newMach, signals) = stepMachine mach event
            in do
                yield signals
                wrapMachine newMach

-- 56 42 +
-- と入力する
calcConduit :: IO ()
calcConduit = do
    -- バッファをオフに設定
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runResourceT process

-- 使い方がよくわからない。。。
sinkTMVar :: MonadIO m => TMVar a -> Sink a m ()
sinkTMVar tmv = forever $ do
    v <- await
    case v of
        Nothing -> return ()
        Just v' -> liftIO (atomically $ putTMVar tmv v')

whyTMVar :: MonadIO m => Source (ResourceT IO) a -> Source (ResourceT IO) a -> Source m a
whyTMVar src1 src2 = do
    t1 <- liftIO newEmptyTMVarIO 
    t2 <- liftIO newEmptyTMVarIO
    void $ liftIO $ async $ fstProc t1
    void $ liftIO $ async $ sndProc t2
    forever $ liftIO (atomically $ takeTMVar t1 `orElse` takeTMVar t2) >>= yield
    
    where
        fstProc t = runResourceT $ src1 $$ sinkTMVar t
        sndProc t = runResourceT $ src2 $$ sinkTMVar t
