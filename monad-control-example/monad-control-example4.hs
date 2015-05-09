{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}


import Control.Exception as E
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Typeable
import System.IO

data MyException = MyException deriving (Show, Typeable)
instance Exception MyException

-- http://www.yesodweb.com/blog/2011/08/monad-control
-- にある説明

withMyFile :: (Handle -> IO a) -> IO a
withMyFile = withFile "test.txt" WriteMode

sayHi :: Handle -> IO ()
sayHi handle = hPutStrLn handle "Hi there"

useMyFile :: IO ()
useMyFile = withMyFile sayHi

sayHiError :: Handle -> ExceptT MyException IO ()
sayHiError handle = do
    lift $ hPutStrLn handle "Hi there, error!"
    throwE MyException

{-
useMyFile は IO () なので成立するが
下記は最終的にIOｄラップされないため定義できない

useMyFileErrorBad :: ExceptT MyException IO ()
useMyFileErrorBad = withMyFile sayHiError
-}

-- 解決策1
-- 内部関数で実装
-- モナドをほどいて実行して再度包む
useMyFileErrorA :: ExceptT MyException IO ()
useMyFileErrorA =
    let unwrapped :: Handle -> IO (Either MyException ())
        unwrapped handle = runExceptT $ sayHiError handle
        applied :: IO (Either MyException ())
        applied = withMyFile unwrapped
        rewrapped :: ExceptT MyException IO ()
        rewrapped = ExceptT applied
     in rewrapped

-- liftWith を使った解決
useMyFileError :: ExceptT MyException IO ()
useMyFileError = do
    x <- liftWith $ \run -> do
      withMyFile $ run . sayHiError
    restoreT $ return x

-- MonadBaseControlも
sayHiCrazy :: Handle -> ReaderT Int (StateT Double (ExceptT MyException IO)) ()
sayHiCrazy handle = liftIO $ hPutStrLn handle "Madness!"
useMyFileCrazy :: ReaderT Int (StateT Double (ExceptT MyException IO)) ()
useMyFileCrazy = control $ \run -> withMyFile $ run . sayHiCrazy

main :: IO ()
main = do
  e <- runExceptT useMyFileErrorA
  print e
  e <- runExceptT useMyFileError
  print e
  e <- runExceptT $ runStateT (runReaderT useMyFileCrazy 0) 0.0
  print e
  putStrLn "end"
