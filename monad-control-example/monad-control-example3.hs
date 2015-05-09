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
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe
import qualified Data.Map as Map

-- MonadTransControl が1つだけ持ち上げるというから試してみたが
-- だめだった

type Name  = String
type Value = String
type Env   = Map.Map Name Value

newtype EvalE m a = EvalE {runEvalE :: ExceptT String m a}
  deriving (Functor
          , Applicative
          , Monad
          , MonadError String
          , MonadBase base
          )
instance MonadTrans EvalE where
  lift = EvalE . lift

instance MonadTransControl EvalE where
  type StT EvalE a = StT (ExceptT String) a
  liftWith = defaultLiftWith EvalE runEvalE
  restoreT = defaultRestoreT EvalE

instance (MonadBaseControl b m) => MonadBaseControl b (EvalE m) where
  type StM (EvalE m) a = ComposeSt EvalE m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

evalET :: (Monad (t []), MonadTransControl t) => t [] Int
evalET = return 1
evalEM :: (MonadBaseControl [] m) => m Int
evalEM = return 1

doEvalET :: EvalE [] Int -> EvalE [] Int
doEvalET t = liftWith (\run -> run t) >>= restoreT . return
doEvalEM :: EvalE [] Int -> EvalE [] Int
doEvalEM m = liftBaseWith (\run -> run m) >>= restoreM

exEvalE  :: EvalE [] Int -> IO ()
exEvalE  = print . runEvalE
exEvalET :: EvalE [] Int -> IO ()
exEvalET = print . runEvalE . doEvalET
exEvalEM :: EvalE [] Int -> IO ()
exEvalEM = print . runEvalE . doEvalEM

newtype EvalR m a = EvalR {runEvalR :: ReaderT Env (EvalE m) a}
  deriving (Functor
          , Applicative
          , Monad
          , MonadError String
          , MonadReader Env
          , MonadBase base
          )
instance MonadTrans EvalR where
  lift = EvalR . lift . lift

-- 全部持ち上げないとだめだった
instance MonadTransControl EvalR where
  type StT EvalR a = StT EvalE (StT (ReaderT Env) a) 
  liftWith f = EvalR $ liftWith $ \runReader ->
                          liftWith $ \runEval ->
                            f $ runEval . runReader . runEvalR 
  restoreT = EvalR . restoreT . restoreT

instance (MonadBaseControl b m) => MonadBaseControl b (EvalR m) where
    type StM (EvalR m) a = ComposeSt EvalR m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

env :: Env
env = Map.fromList [("0", "zero"), ("1", "one")]

doEvalRT :: EvalR [] Int -> EvalR [] Int
doEvalRT t = do
    x <- liftWith $ \r ->
      r t
    restoreT $ return x
doEvalRM :: EvalR [] Int -> EvalR [] Int
doEvalRM m = liftBaseWith (\run -> run m) >>= restoreM

exEvalR  :: EvalR [] Int -> IO ()
exEvalR  = print . runEvalE . (flip runReaderT env) . runEvalR
exEvalRT :: EvalR [] Int -> IO ()
exEvalRT = print . runEvalE . (flip runReaderT env) . runEvalR . doEvalRT
exEvalRM :: EvalR [] Int -> IO ()
exEvalRM = print . runEvalE . (flip runReaderT env) . runEvalR . doEvalRM

main :: IO ()
main = do
    putStrLn "end"
