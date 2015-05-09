{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad

-- 全てのMonadをFunctorにする
-- このファイルをimportした全てに影響があるので危険
instance (Monad m) => Functor m where
    fmap = liftM

data Packet a = Wrap a
    deriving (Show, Eq)

instance Monad Packet where
    return x = Wrap x
    (Wrap x) >>= f = f x

main :: IO ()
main = do
  putStrLn "end"
