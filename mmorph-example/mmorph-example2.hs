{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}


import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Morph

newtype EvalE m a = EvalE {runEvalE :: ExceptT String m a}
  deriving (Functor
          , Applicative
          , Monad
          , MonadError String
          )
instance MonadTrans EvalE where
  lift = EvalE . lift

instance MFunctor EvalE where
  hoist f (EvalE m) = EvalE $ hoist f m

instance MMonad EvalE where
    embed f m = embed f m

newtype EvalW m a = EvalW {runEvalW :: WriterT String m a}
  deriving (Functor
          , Applicative
          , Monad
          , MonadWriter String
          )
instance MonadTrans EvalW where
  lift = EvalW . lift

instance MFunctor EvalW where
  hoist f (EvalW m) = EvalW $ hoist f m

instance MMonad EvalW where
    embed f m = embed f m

main :: IO ()
main = do
  putStrLn "end"
