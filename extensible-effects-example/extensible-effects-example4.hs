{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Exception
import Control.Eff.Choose
import Control.Eff.Cut
import Control.Eff.Lift
import Control.Monad

import Data.Void

{- Reader -}
up1 :: Member (Reader Int) r => Eff r Int
up1 = do 
  v <- ask
  return (v + 1:: Int)
up2 :: Eff (Reader Int :> r) Int
up2 = do 
  v <- ask
  return (v + 1:: Int)
showReader :: IO ()
showReader = do
  putStrLn "--Eff.Reader--"
  print $ run $ runReader up1 (10::Int)
  print $ run $ runReader up2 (10::Int)

{- Choose -}
add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

upList1 :: Member Choose r => Eff r Int
upList1 = return 1 `add` choose [1,2,3]

upList2 :: Eff (Choose :> r) Int
upList2 = return 1 `add` choose [1,2,3]

showChoose :: IO ()
showChoose = do
  putStrLn "--Eff.Choose--"
  print $ run $ runChoice upList1
  print $ run $ runChoice upList2

{- Cut -}
-- The signature is inferred
exCut11 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
exCut11 =          (return (1::Int) `mplus'` return 2) 
          `mplus'` ((cutfalse `mplus'` return 4) `mplus'` return 5)
exCut21 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
exCut21 =          return (1::Int) 
          `mplus'` call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'` return 4)
          `mplus'` return 5
exCut31 :: (Member Choose r, Member (Exc CutFalse) r) => Eff r Int
exCut31 =          call exCut11 
          `mplus'` call (exCut21 `mplus'` cutfalse)
-- [1,2,1,2,5]

exCut12 :: Member Choose r => Eff (Exc CutFalse :> r) Int
exCut12 =          (return (1::Int) `mplus'` return 2) 
          `mplus'` ((cutfalse `mplus'` return 4) `mplus'` return 5)
exCut22 :: Member Choose r => Eff (Exc CutFalse :> r) Int
exCut22 =          return (1::Int) 
          `mplus'` call (return 2 `mplus'` (cutfalse `mplus'` return 3) `mplus'`  return 4)
          `mplus'` return 5
exCut32 :: Member Choose r => Eff (Exc CutFalse :> r) Int
exCut32 = call exCut12 `mplus'` call (exCut22 `mplus'` cutfalse)
showCut :: IO ()
showCut = do
  putStrLn "--Eff.Cut--"
  print $ run . runChoice $ call exCut11
  print $ run . runChoice $ call exCut21
  print $ run . runChoice $ call exCut31
  print $ run . runChoice $ call exCut12
  print $ run . runChoice $ call exCut22
  print $ run . runChoice $ call exCut32

{- Lift -}
exLift1 :: (SetMember Lift (Lift IO) r, Member (Reader Int) r) => Eff r ()
exLift1 = ask >>= \(x::Int) -> lift . print $ x

exLift2 :: Eff (Reader Int :> Lift IO :> ()) ()
exLift2 = ask >>= \(x::Int) -> lift . print $ x

showLift :: IO ()
showLift = do
  putStrLn "--Eff.Lift--"
  runLift (runReader exLift1 (5::Int))
  runLift (runReader exLift2 (5::Int))
  -- 5

{- State -}
exState11 :: Member (State Int) r => Eff r Int
exState11 = do
  put (10 ::Int)
  x <- get
  return (x::Int)
exState21 :: Member (State Int) r => Eff r Int
exState21 = do
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y)

exState12 :: Eff (State Int :> r) Int
exState12 = do
  put (10 ::Int)
  x <- get
  return (x::Int)
exState22 :: Eff (State Int :> r) Int
exState22 = do
  put (10::Int)
  x <- get
  put (20::Int)
  y <- get
  return (x+y)

showState :: IO ()
showState = do
  putStrLn "--Eff.State--"
  print $ run $ runState (0::Int) exState11
  print $ run $ runState (0::Int) exState21
  print $ run $ runState (0::Int) exState12
  print $ run $ runState (0::Int) exState22

main :: IO ()
main = do
  showReader
  showChoose
  showCut
  showLift
  showState

