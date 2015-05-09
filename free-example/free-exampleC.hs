{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ConstraintKinds           #-}

import Control.Applicative
import Control.Monad.Trans.Free
import Control.Monad.State
import Data.Maybe
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import GHC.Exts(Constraint)

type Name = String

data Var v a = Ref Name (v -> a ) | Sub Name v a
  deriving (Functor)

type ForbiddenT v = FreeT (Var v)
type Forbidden v = FreeT (Var v) Identity

ref :: (Show k,MonadFree (Var v) m) => k -> m v
ref k = liftF (Ref (show k) id)

(?=) :: (Show k, MonadFree (Var v) m) => k -> v -> m () 
k ?= v = liftF $ Sub (show k) v ()

runForbidden :: Forbidden v a -> a
runForbidden = runIdentity . runForbiddenT

runForbiddenT :: Monad m => ForbiddenT v m a -> m a
runForbiddenT = run M.empty where
  run table ft = runFreeT ft >>= \ff -> case ff of
    Pure x                 -> return x
    Free (Ref name cont)   -> run table $ cont $ fromMaybe undefined $ M.lookup name table
    Free (Sub name v cont) -> run (M.insert name v table) cont

ex :: Forbidden Int Int
ex = do
  "x" ?= 0
  "y" ?= 3
  "x" ?= 2
  (+) <$> ref "x" <*> ref "y"

fact :: Int -> Forbidden Int Int
fact n = do
  "x" ?= 1 
  forM [1..n] $ \i -> do
    nx <- (*i) <$> ref "x"
    "x" ?= nx 
  ans <- ref "x"
  return ans

swap k1 k2 = do
  x <- ref k1
  y <- ref k2
  k1 ?= y
  k2 ?= x

ex2 :: MonadIO m => ForbiddenT Int m ()
ex2 = do
  "x" ?= 10
  "y" ?= 20
  swap "x" "y"
  x <- ref "x"
  y <- ref "y"
  liftIO $ print x
  liftIO $ print y

shadow :: MonadIO m => ForbiddenT Int m ()
shadow = do
  "x" ?= 2
  x <- ref "x"
  liftIO $ print x -- => 2
  runForbiddenT $ do
    "x" ?= 3
    x <- ref "x"
    liftIO $ print x -- => 3
  liftIO $ print x -- => 2

-- なんでも入る版

data Nue = forall a. (Read a, Show a) => Nue a
type Nuey k v m = (Show k, Read v, Show v, MonadFree (Var Nue) m)

-- refのNue版
thirdEye :: Nuey k v m => k -> m v
thirdEye k = liftF (Ref (show k) id) >>= return . unNue
  where unNue (Nue v) = safeRead $ show v
        safeRead      = fromMaybe undefined . maybeRead
        maybeRead     = fmap fst . listToMaybe . reads

-- (?=)のNue版
(!=) :: Nuey k v m => k -> v -> m ()
k != v = liftF $ Sub (show k) (Nue v) ()

ex3 :: MonadIO m => ForbiddenT Nue m ()
ex3 = do
  "pi"  != 3.1415
  "ten" != 10
  pi  :: Double   <- thirdEye "pi"
  ten :: Int      <- thirdEye "ten"
  ten_f :: Double <- thirdEye "ten"
  liftIO $ print pi -- => 3.1415..
  liftIO $ print ten -- => 10
  liftIO $ print ten_f -- => 10.0
  "ten" != "dynamic typing"
  destroyedTen :: String <- thirdEye "ten"
  liftIO $ print destroyedTen -- => "dynamic typing"

main :: IO ()
main = do
  print $ runForbidden ex
  print $ runForbidden $ fact 5
  runForbiddenT ex2
  runForbiddenT shadow
  runForbiddenT ex3
  putStrLn "end"
