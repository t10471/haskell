module Mydebug(
    (.$.), 
    (...),
    d,
    d'
    ) where


import Debug.Trace

infixr 0 .$.
(.$.) :: Show a => (a -> b) -> a -> b
f .$. x = trace (show x) f x

infixr 9 ...
(...) :: Show b => (b -> c) -> (a -> b) -> (a -> c)
f ... g = (f .$.) . g 

d :: Show a => a -> a
d x = trace (show x) x

d' :: (Show a, Show b) => a -> b -> b
d' s x  = trace ((show s) ++ (show x)) x