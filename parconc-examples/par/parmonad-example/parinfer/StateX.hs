{-# LANGUAGE ViewPatterns #-}
--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module StateX (StateX, ddd, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX) where

data  StateX s a      =   MkSX (s -> a)
rep (MkSX f)          =   f
returnSX returnX x    =   MkSX (\s -> returnX (x, s))
eachSX eachX xSX f    =   MkSX (\s -> rep xSX s `eachX` (\(x,s') -> (f x, s')))
thenSX thenX xSX kSX  =   MkSX (\s -> rep xSX s `thenX` (\(x,s') -> rep (kSX x) s'))
toSX eachX xX         =   MkSX (\s -> xX `eachX` (\x -> (x,s)))
putSX returnX s'      =   MkSX (\s -> returnX ((), s'))
getSX returnX         =   MkSX (\s -> returnX (s,s))
useSX eachX s xSX     =   rep xSX s `eachX` (\(x,s') -> x)

ddd (MkSX f)          =   f

add :: (Num a) => (a,a) -> a
add (x, y) = x + y
xx :: a -> (a -> b) -> b
xx x f = f x
xxx :: (a, b) ->((a,b) -> c) -> c
xxx (x, y) f = f (x, y)
add1 :: (Num a) => a -> a
add1 x = x + 1
add1t :: (Num a) => (a, a) -> (a, a)
add1t (x, y) = (x+1, y+1)
add1t' :: (Num b) => (a, b) -> (a, b)
add1t' (x, y) = (x, y+1)

main :: IO ()
main = do
  -- MkSX :: (s -> a) -> StateX s a
  print $ (++) "rep (MkSX add1) 1 = "  $ show $ rep (MkSX add1) 1
  -- => 2
  print $ (++) "rep (MkSX add) (1,2) = " $ show $ rep (MkSX add) (1,2)
  -- => 3
  -- returnSX :: ((t, t1) -> a) -> t -> StateX t1 a
  print $ (++) "rep (returnSX id  1) 2 = " $ show $ rep (returnSX id  1) 2 
  -- => (1,2)
  print $ (++) "rep (returnSX add 1) 2 = " $ show $ rep (returnSX add 1) 2 
  -- => 3
  -- eachSX :: (t1 -> ((t2, t4) -> (t3, t4)) -> a) -> StateX t t1 -> (t2 -> t3) -> StateX t a
  print $ (++) "rep (eachSX xxx (returnSX add1t 1) add1) 2 = " $ show $ rep (eachSX xxx (returnSX add1t 1) add1) 2 
  -- => (3,3) == add1t (add1 1,2)
  -- thenSX :: (t1 -> ((t2, t3) -> t4) -> a) -> StateX t t1 -> (t2 -> StateX t3 t4) -> StateX t a
  print $ (++) "rep (thenSX xxx (returnSX add1t 1) (\\x -> returnSX add1t x)) 2 = " $ show $ rep (thenSX xxx (returnSX add1t 1) (\x -> returnSX add1t x)) 2 
  -- => (3,4) == add1t(add1t (1,2))
  -- toSX :: (t -> (t1 -> (t1, t2)) -> a) -> t -> StateX t2 a
  print $ (++) "$ rep (toSX xx 1) 2 = " $ show $ rep (toSX xx 1) 2
  -- => (1,2)
  -- putSX :: (((), t) -> a) -> t -> StateX s a
  print $ (++) "rep (putSX id 1) 2 = " $ show $ rep (putSX id 1) 2
  -- => ((),1)
  print $ (++) "(putSX (\\(x,y) -> y) 1) 2 = " $ show $ rep (putSX (\(x,y) -> y) 1) 2
  -- => 1
  print $ (++) "rep (putSX (\\(x,y) -> x) 1) 2 = " $ show $ rep (putSX (\(x,y) -> x) 1) 2
  -- => ()
  print $ (++) "rep (rep (putSX (returnSX add1t') 1) 2) 3 = " $ show $ rep (rep (putSX (returnSX add1t') 1) 2) 3 
  -- => (((),1),4)
  print $ (++) "rep (rep (putSX (returnSX (\\(x, y) -> y)) 1) 2) 3 = " $ show $ rep (rep (putSX (returnSX (\(x, y) -> y)) 1) 2) 3 
  -- => ((),1)
  print $ (++) "(rep (putSX (returnSX (\\(x, y) -> x)) 1) 2) 3 = " $ show $ rep (rep (putSX (returnSX (\(x, y) -> x)) 1) 2) 3 
  -- => 3
  print $ (++) "rep (rep (putSX (returnSX id) 1) 2) 3 = " $ show $ rep (rep (putSX (returnSX id) 1) 2) 3 
  -- => (((),1),3)
  -- getSX returnX         =   MkSX (\s -> returnX (s,s))
  print $ (++) "rep (getSX id) 1 = " $ show $ rep (getSX id) 1 
  -- => (1,1)
  print $ (++) "rep (rep (getSX (returnSX id)) 1) 2 = " $ show $ rep (rep (getSX (returnSX id)) 1) 2
  -- => ((1,1),2)
  -- useSX eachX s xSX     =   rep xSX s `eachX` (\(x,s') -> x)
  print $ (++) "useSX xxx 1 (returnSX add1t 2) = " $ show $ useSX xxx 1 (returnSX add1t 2)
  -- => 3 == xxx (rep (returnSX add1t 2) 1) (\(x,s') -> x)
