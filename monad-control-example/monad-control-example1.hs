{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Control
import Control.Exception
import Debug.Trace

mti :: MaybeT [] String
mti = return "abc"

double :: [a] -> [a]
double []     = []
double (x:xs) = x : x : (double xs)

{-
説明1
listWith がやっていることは下記の処理の抽象化
モナドトランスファーの処理の途中で中身が欲しくなったときに
XXXT . func . runXXXTとしないといけないのを抽象化する仕組み
 -}
doubleMaybeT :: MaybeT [] a -> MaybeT [] a
doubleMaybeT = MaybeT . double . runMaybeT

doubleT :: (Monad (t [] ), MonadTransControl t) => t [] a -> t [] a
doubleT t = do
    x <- liftWith $ \run -> do
      -- runで実行した結果を貰い処理(double)し
      double $ run t
    -- 包んで返す
    restoreT $ return x
-- 1行で書いた場合
doubleT' :: (Monad (t [] ), MonadTransControl t) => t [] a -> t [] a
doubleT' t = liftWith (\run -> double $ run t) >>= restoreT . return

doubleM :: (MonadBaseControl [] m) => m a -> m a
doubleM t = do
    x <- liftBaseWith $ \run -> do
      double $ run t
    restoreM x

exTMti :: MaybeT [] String
exTMti = do
    x <- liftWith $ \run -> do
      run $ doubleT mti
    restoreT $ return x 

exMMti :: MaybeT [] String
exMMti = do
    x <- liftBaseWith $ \run -> do
      run $ doubleM mti
    restoreM x
-- 説明1終わり

{-
説明2
 https://www.fpcomplete.com/user/jwiegley/monad-control
 ネストしたモナド操作の一般化に
 MonadBaseControl を使用したサンプル
 -}
stfn :: StateT Int IO String
stfn = do
    modify (+1)
    show <$> get

-- 単純な例
exSimple :: IO ()
exSimple = do
    (x,y) <- runStateT stfn 0
    print $ "x = " ++ show x   -- x = "1"
    (x',y') <- runStateT stfn y
    print $ "x = " ++ show x'  -- x = "2"
-- ネストして操作する場合
exNest :: IO ()
exNest = do
    (x',y') <- flip runStateT 0 $ fooIO stfn
    print $ "x = " ++ show x'   -- x = "2"
fooIO :: StateT Int IO String -> StateT Int IO String
fooIO f = do
    x <- f
    y <- get
    y' <- liftIO $ do
        print $ "x = " ++ show x   -- x = "1"
        (x',y') <- runStateT f y
        return y'
    return $ show y'
-- liftIO を使っているのを一般化したり、
-- 外のトランスフォーマーモナドの依存を取り除ける
exControl :: IO ()
exControl = do
    (x',y') <- flip runStateT 0 $ foo stfn
    print $ "x = " ++ show x'   -- x = "2"
foo :: MonadBaseControl IO m => m String -> m String
foo f = do
    x <- f
    y' <- liftBaseWith $ \runInIO -> do
        print $ "x = " ++ show x   -- x = "1"
        x' <- runInIO f
        return x'
    restoreM y'
-- 単に関数を実行するだけならこれでOK
foo' :: MonadBaseControl IO m => m String -> m String
foo' f = liftBaseWith (\runInIO -> runInIO f) >>= restoreM
-- control をつかうとさらに短くかける
foo'' :: MonadBaseControl IO m => m String -> m String
foo'' f = control $ \runInIO -> runInIO f
-- 要はcatchがIO固定なのを取り除きたいってのが目的で下がその例
-- ただし、Control.Exception.Lifted 実際はあるので使うときはそっちを
ctlCatch :: (MonadBaseControl IO m, Exception e) => m a -> (e -> m a ) -> m a
ctlCatch f h = control $ \runInIO -> catch (runInIO f) (runInIO . h)
{-
-- 説明2終わり
-}

{-
-- 説明3
-- maybeTIO と maybeMIO は入れ替え可能？
-- 型が違うだけ？
-}
maybeTIO :: (Monad (t IO), MonadTransControl t) => t IO Int
maybeTIO = lift     (putStrLn "testMaybeT") >> return 1
maybeMIO :: (MonadBaseControl IO m) => m Int
maybeMIO = liftBase (putStrLn "testMaybeT") >> return 1

twiceIO :: IO a -> IO a
twiceIO action = action >> action

testLiftWith     :: MaybeT IO Int -> MaybeT IO Int  
testLiftWith     f = liftWith     (\run -> twiceIO (run f)) >>= restoreT . return
testLiftBaseWith :: MaybeT IO Int -> MaybeT IO Int
testLiftBaseWith f = liftBaseWith (\run -> twiceIO (run f)) >>= restoreM
testControl      :: MaybeT IO Int -> MaybeT IO Int
testControl      f = control (\run -> twiceIO (run maybeTIO))
               
exTest :: IO ()
exTest = do
  runMaybeT (testLiftWith     maybeTIO) >>= print
  runMaybeT (testLiftWith     maybeMIO) >>= print
  runMaybeT (testLiftBaseWith maybeTIO) >>= print
  runMaybeT (testLiftBaseWith maybeMIO) >>= print
  runMaybeT (testControl      maybeTIO) >>= print
  runMaybeT (testControl      maybeMIO) >>= print
-- 説明3終わり

main :: IO ()
main = do
  exTest
  putStrLn "end"
