{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EX (Packet(..)) where

import Control.Applicative
import Control.Monad

instance (Monad m) => Functor m where
    fmap = liftM

instance (Monad m) => Applicative m where
    pure  = return
    (<*>) = ap

data Packet a = Wrap a
    deriving (Show, Eq)

instance Monad Packet where
    return x = Wrap x
    (Wrap x) >>= f = f x

main :: IO ()
main = do
  putStrLn "end"
