{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad (ap)

-- Nat(H.F, G) = Nat(H, G^F)
-- のHaskell
-- 指数関手

data Exp f g x = Exp (forall y. (x -> f y) -> g y)

phi :: Functor h => (forall x. h (f x) -> g x) -> h y -> Exp f g y 
phi t y = Exp $ \k -> t (fmap k y)

cophi :: (forall y. h y -> Exp f g y) -> h (f x) -> g x
cophi t x = let Exp g = t x in g id

-- F* X = X + F(F* X)
-- フリーモナド

data Free f x = Ret x | Con (f (Free f x))

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Ret a)  = Ret (f a )
    go (Con fa) = Con (go <$> fa)

instance Functor f => Applicative (Free f) where
  pure = Ret 
  Ret a  <*> Ret b  = Ret $ a b
  Ret a  <*> Con mb = Con $ fmap a  <$> mb
  Con ma <*> b      = Con $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  return = Ret
  Ret x >>= f = f x
  Con m >>= f = Con $ fmap (>>= f) m

ins :: Functor f => f x -> Free f x
ins x = Con $ fmap Ret x

free :: (Functor f, Monad m) => (f (m x) -> m x) -> Free f x -> m x
free f (Ret x) = return x
free f (Con t) = f (fmap (free f) t)

type Rep f = Exp f f

instance Functor (Rep f) where
  fmap f (Exp m) = Exp $ \k -> m (k . f)

instance Functor f => Applicative (Rep f) where
  pure x = Exp $ \k -> k x
  (<*>)  = ap

instance Functor f => Monad (Rep f) where
  return = pure
  Exp m >>= f = Exp $ \h -> m $ \x -> let Exp t = f x in t h

rep :: Monad m => m x -> Rep m x
rep m = Exp (m >>=)

abs :: Monad m => Rep m x -> m x
abs (Exp m) = m return

main :: IO ()
main = putStrLn "end"
