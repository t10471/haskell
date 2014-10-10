
{-
関数も Functor 
instance Functor ((->) r) where  
    fmap f g = f . g
    関数に fmap を使用すると、関数の合成(function composition)をしたのと同じ

fmap             ::         Functor f  => (a -> b) ->  f    a  ->  f    b
fmap (*3)        :: (Num b, Functor f) =>              f    b  ->  f    b
fmap (*3) (+100) ::  Num b             =>                   b  ->       b
     (.)         ::                       (b -> c) -> (a -> b) -> (a -> c)
((*3) .)         ::  Num c             =>             (a -> c) -> (a -> c)
 (*3) .   (+100) ::  Num c             =>                   c  ->       c
以下の等式が成り立っている？
f a == (a -> b)
f b == (a -> c)
-}

main = do
          -- 3 * (100 + 1) 
          print $ fmap (*3) (+100) 1 
          -- 100 + (3 * 1) 
          print $ fmap (+100) (*3) 1
