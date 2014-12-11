--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module InferMonad     (Infer, returnI, eachI, thenI, guardI, useI, getSubI,
                       substituteI, unifyI, freshI, freshesI, mmm)
                      where

import MaybeM
import StateX         (ddd, StateX, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX)
import Type
import Substitution
import MyDebug
import Debug.Trace

type  Counter         =  Int
data  Infer x         =  MkI (StateX Sub (StateX Counter (Maybe ((x, Sub), Counter))))
rep (MkI xJ)          =  xJ
returnI               :: x -> Infer x
returnI x             =  MkI (returnSX (returnSX returnM) x)
eachI                 :: Infer x -> (x -> y) -> Infer y
xI `eachI` f          =  MkI (eachSX (eachSX eachM) (rep xI) f)
thenI                 :: Infer x -> (x -> Infer y) -> Infer y
xI `thenI` kI         =  MkI (thenSX (thenSX thenM) (rep xI) (rep . kI))
failI                 :: Infer x
failI                 =  MkI (toSX (eachSX eachM) (toSX eachM failM))
-- 2つのStateXとMaybeをはずすためにuseSXとuseMをする
useI                  :: x -> Infer x -> x
useI xfail            =  useM xfail
                      .  useSX eachM 0
                      .  useSX (eachSX eachM) emptySub
                      .  rep
guardI                :: Bool -> Infer x -> Infer x
guardI b xI           =  if  b  then  xI  else  failI
putSubI               :: Sub -> Infer ()
putSubI s             =  MkI (putSX (returnSX returnM) s)
getSubI               :: Infer Sub
getSubI               =  MkI (getSX (returnSX returnM))
putCounterI           :: Counter -> Infer ()
putCounterI c         =  MkI (toSX (eachSX eachM) (putSX returnM c))
getCounterI           :: Infer Counter
getCounterI           =  MkI (toSX (eachSX eachM) (getSX returnM))
substituteI           :: MonoType -> Infer MonoType
substituteI t         =  getSubI              `thenI`  (\ s ->
                         returnI  (
                          (debug "substituteI applySub s t = " 
                            (applySub 
                              (debug "substituteI s = " s)
                              (debug "substituteI t = " t)
                            ))
                         ))
unifyI                :: MonoType -> MonoType -> Infer ()
unifyI t u            =  getSubI              `thenI`  (\ s  ->
			 let sM = unifySub (debug "unifyI t = " t) (debug "unifyI u = " u) (debug "unifyI s = " s)
			 in
                         existsM sM           `guardI` (
                         putSubI (theM (debug "unifyI sM = " sM))    `thenI`  (\ () ->
                                              returnI  ())))
-- 関数の型の数字を決める a1 -> a1などの
-- カウンターみたいなもの
freshI                :: Infer MonoType
freshI                =  getCounterI          `thenI` (\c  ->
                         putCounterI (c+1)    `thenI` (\() ->
                                              returnI (TVar ("a" ++ show c))))
freshesI              :: Int -> Infer [MonoType]
freshesI 0            =                       returnI []
freshesI n            =  freshI               `thenI` (\x  ->
                         freshesI (n-1)       `thenI` (\xs ->
                                              returnI (x:xs)))

mmm (MkI xJ)          =  xJ

instance Monad Infer where
  return = returnI
  (>>=) = thenI

instance Functor Infer where
  fmap f x = x >>= return . f

main :: IO()
main = do
  let pp  = \x -> useM (([TVar "faild"], emptySub), 0) (ddd (ddd (rep (x)) emptySub) 0)
  let pp2 = \x -> useM ((9999, emptySub), 0) (ddd (ddd (rep (x)) emptySub) 0)
  print $ pp $
    freshI           >>= (\x0  ->
    freshI           >>= (\x1  ->
    freshI           >>= (\x2  ->
    returnI[]        >>= (\xs0 ->
    returnI (x2:xs0) >>= (\xs1 ->
    returnI (x1:xs1) >>= (\xs2 ->
    returnI (x0:xs2)))))))
  -- (([a0,a1,a2],[]),3)
  print $ pp2 $
    getCounterI        >>= (\c1 ->
    putCounterI (c1+1) >>= (\() ->
    returnI (c1)))
-- ((0,[]),1)
  print $ pp2 $
    getCounterI        >>= (\c1 ->
    putCounterI (c1+1) >>= (\() ->
    getCounterI        >>= (\c2 ->
    putCounterI (c2+1) >>= (\() ->
    returnI (c2)))))
-- ((1,[]),2)
  print $ pp2 $
    getCounterI        >>= (\c1 ->
    putCounterI (c1+1) >>= (\() ->
    getCounterI        >>= (\c2 ->
    putCounterI (c2+1) >>= (\() ->
    getCounterI        >>= (\c3 ->
    putCounterI (c3+1) >>= (\() ->
    returnI (c3)))))))
-- ((2,[]),3)
  print $ pp2 $
    getCounterI        >>= (\c1 ->
    putCounterI (c1+1) >>= (\() ->
    getCounterI        >>= (\c2 ->
    putCounterI (c2+1) >>= (\() ->
    getCounterI        >>= (\c3 ->
    putCounterI (c3+1) >>= (\() ->
    returnI (c3)))))))
  let a0 = TVar "a0"
  let a1 = TVar "a1"
  let a2 = TVar "a2"
  let a3 = TVar "a3"
  let a4 = TVar "a4"
  let m1 =  TCon "->" [a1, a2]
  let m2 =  TCon "->" [m1, a4]
  let m3 =  TCon "->" [a1, a2, a4]
  print $ (++) "m2 = " $ show $ m2
  -- "m2 = (a1 -> a2) -> a4"
  print $ (++) "m3 = " $ show $ m3
  -- print $ pp3 $
    -- unifyI a0 (a1 `arrow` a3)
  print $ a0 `arrow` a1 `arrow` a3
  let mm1 = TCon "->" [a0, a1 `arrow` a3]
  print mm1
  let mm2 = TCon "->" [mm1, mm1]
  print mm2
