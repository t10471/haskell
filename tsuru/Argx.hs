{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Arg (
      Env(..)
    , Args(..)
    , EnvT(..)
    , parseArgs
) where 

import Control.Monad.Trans.Except 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Directory

data Env = Env {
              file      :: FilePath
            , isReOrder :: Bool
            } deriving Show
type Args = [String]

type EnvT a = ReaderT  Args (ExceptT String
                           (WriterT [String] (StateT Env IO))) a

runEnvT :: Args -> Env -> EnvT a -> IO ((Either String a, [String]), Env)
runEnvT args env ev =
      runStateT (runWriterT (runExceptT (runReaderT ev args))) env

initEnv = Env {file = "", isReOrder = False}
maxLenght = 2
rFlg = "-r"
remove t []     = []
remove t (x:xs) = rm t x xs
  where rm t x xs
          | t == x    = remove t xs
          | otherwise = x : remove t xs 

onReOrder :: EnvT ()
onReOrder = do
  x <- get
  put x {isReOrder = True}

setFile :: String -> EnvT ()
setFile f = do
  x <- get
  put x {file = f}

evalArgs :: EnvT ()
evalArgs = do
    xs <- ask
    when (null xs) $ lift $ throwE "empty args"
    when (length xs > maxLenght) $ lift $ throwE "too many args"
    loop
    return ()
  where
    loop = do
      xs <- ask
      if | elem rFlg xs   -> onReOrder >> local (remove rFlg) loop
         | length xs /= 1 ->  tell ["invalid args"]
         | otherwise      -> check $ head xs
    check f = do
      b <- liftIO $ doesFileExist f
      if b then setFile f else tell ["file not exists"]

parseArgs :: Args -> IO ([String], Env)
parseArgs args = do
  ((ret, msgs), env) <-  runEnvT args initEnv evalArgs 
  case ret of
    Left msg -> return ([msg], env)
    Right d  -> return (msgs, env)
