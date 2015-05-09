{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Trans.Control
import qualified Data.Map as Map
import Control.Exception.Lifted (catch)
import Control.Exception (SomeException)
import System.IO

-- hackage にのっている説明
newtype CounterT m a = CounterT {unCounterT :: StateT Int m a}
  deriving (Functor, Applicative, Monad, MonadIO,  MonadTrans)

deriving instance (MonadBase b m) => MonadBase b (CounterT m)

instance MonadTransControl CounterT where
    type StT CounterT a = StT (StateT Int) a
    liftWith = defaultLiftWith CounterT unCounterT
    restoreT = defaultRestoreT CounterT

instance (MonadBaseControl b m) => MonadBaseControl b (CounterT m) where
    type StM (CounterT m) a = ComposeSt CounterT m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM
-- hackage にのっている説明 終わり

-- catch の使い方
-- http://maoe.hatenadiary.jp/entry/20111207/1323185162
type Env        = Map.Map Int String
type MyAppEnv   = Env
type MyAppState = Integer

newtype MyAppT m a = MyAppT { runMyAppT :: ReaderT MyAppEnv (StateT MyAppState m) a
                            } deriving ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadIO
                                       , MonadState MyAppState
                                       , MonadReader MyAppEnv
                                       , MonadBase base
                                       )

instance MonadTrans MyAppT where
  lift = MyAppT . lift . lift

instance MonadTransControl MyAppT where
  type StT MyAppT a = StT (StateT MyAppState) (StT (ReaderT MyAppEnv) a)
  liftWith f = MyAppT $ liftWith $ \runReader ->
                          liftWith $ \runState ->
                            f $ runState . runReader . runMyAppT 
  restoreT = MyAppT . restoreT . restoreT

instance MonadBaseControl b m => MonadBaseControl b (MyAppT m) where
    type StM (MyAppT m) a = ComposeSt MyAppT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

appNG :: MonadIO m => MyAppT m ()
appNG = do
  liftIO $ putStrLn "Hello, MyApp!"
  liftIO $ do
        h <- openFile "/home/maoe/NoSuchFile" ReadMode
        hGetContents h
        hClose h
    `catch` \(e :: SomeException) -> putStrLn $ "Caught " ++ show e

appIN :: Monad m => MyAppT m Int
appIN = return 1

appOK :: Monad m => MyAppT m Int
appOK = do
  r <- ask
  s <- get
  x <-  liftWith $ \run -> do
    run appIN
  restoreT $ return x

setupMyAppEnv :: MyAppEnv
setupMyAppEnv = Map.fromList [(0, "zero"), (1, "one")]

initialMyAppState :: MyAppState
initialMyAppState = 0

runMyApp :: MyAppT m a -> MyAppState -> MyAppEnv -> m (a, MyAppState)
runMyApp ap st env = runStateT (runReaderT (runMyAppT ap) env) st
-- catch の使い方 終わり



main :: IO ()
main = do
  runMyApp appNG initialMyAppState setupMyAppEnv 
  r <- runMyApp appOK initialMyAppState setupMyAppEnv 
  print r
  putStrLn "end"
