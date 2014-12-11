module MyDebug
       ((.$), (...), debug, debux)
       where

import Debug.Trace

infixr 0 .$
(.$) :: Show a => (a -> b ) -> a -> b
f .$ x = trace (show x) f x

infixr 9 ...
(...) :: Show b => (b -> c ) -> (a -> b ) -> (a -> c )
f ... g = (f .$ ) . g 

debug :: (Show a) => String -> a -> a
debug m x = x
-- debug m x = trace (m ++ show (x)) x
--
debux :: (Show a) => String -> a -> a
debux m x = trace (m ++ show (x)) x
