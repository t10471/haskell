{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}

import Control.Eff
import Control.Eff.State.Lazy
import Control.Eff.Writer.Strict
import Control.Eff.Exception
import Control.Eff.Coroutine
import Control.Eff.Trace
import Control.Eff.Fresh

import Data.Void

{- Conroutine & Trace -}
exYieldI1 :: Member (Yield Int) r => Eff r ()
exYieldI1 = yield (1::Int) >> yield (2::Int)
exYieldF1 :: Member (Yield Float) r => Eff r ()
exYieldF1 = yield (1.0::Float) >> yield (2.0::Float)
exYieldM1 :: (Member (Yield Int) r) => Eff r ()
exYieldM1 = mapM_ yield [1, 2, 3, 4, 5::Int]

exYieldI2 :: Eff (Yield Int :> r) ()
exYieldI2 = yield (1::Int) >> yield (2::Int)
exYieldF2 :: Eff (Yield Float :> r) ()
exYieldF2 = yield (1.0::Float) >> yield (2.0::Float)
exYieldM2 :: Eff (Yield Int :> r) ()
exYieldM2 = mapM_ yield [1, 2, 3, 4, 5::Int]

cI :: Eff (Yield Int :> Trace :> ()) () -> IO ()
cI ex = runTrace (loop =<< runC ex)
 where loop (Y x k)  = trace (show (x::Int)) >> k () >>= loop
       loop (Done _) = trace "Done"

cF :: Eff (Yield Float :> Trace :> ()) () -> IO ()
cF ex = runTrace (loop =<< runC ex)
 where loop (Y x k)  = trace (show (x::Float)) >> k () >>= loop
       loop (Done _) = trace "Done"


showCoroutine :: IO ()
showCoroutine = do
  putStrLn "--Eff.Coroutine--"
  cI exYieldI1 >> cF exYieldF1
  cI exYieldM1
  cI exYieldI2 >> cF exYieldF2
  cI exYieldM2

{- Exception -}
-- exceptions and state
exS1 :: Member (State Int) r => Eff r ()
exS1 = get >>= put . (+ (1::Int))

exErr1 :: (Member (State Int) r, Member (Exc [Char]) r) => Eff r b
exErr1 = do
  exS1
  throwExc "exc"

exS2 :: Eff (State Int :> r) ()
exS2 = get >>= put . (+ (1::Int))

exErr2 :: Eff (State Int :> Exc [Char] :> r) b
exErr2 = do
  exS2
  throwExc "exc"

showException :: IO ()
showException = do
  putStrLn "--Eff.Exception--"
  print $ (run $ runState (1::Int) $ runExc exErr1 :: (Int, Either String String))
-- (2, Left "exc")
  print $ (run $ runExc $ runState (1::Int) exErr1 :: Either String (Int, String))
-- Left "exc"
  -- exErr2 :: Eff (Exc [Char] :> State Int :> r) bでないとだめ
  -- print $ (run $ runState (1::Int) $ runExc exErr2 :: (Int, Either String String))
  print $ (run $ runExc $ runState (1::Int) exErr2 :: Either String (Int, String))

{- Fresh -}
exFresh1 :: Member Trace r => Eff r ()
exFresh1 = flip runFresh (f 0) $ do
  (n1::Int) <- fresh
  trace $ "Fresh " ++ show n1
  (n2::Int) <- fresh
  trace $ "Fresh " ++ show n2
    where
      f :: Int -> Int
      f a = a

exFresh2 :: Eff (Trace :> r) ()
exFresh2 = flip runFresh (f 0) $ do
  (n1::Int) <- fresh
  trace $ "Fresh " ++ show n1
  (n2::Int) <- fresh
  trace $ "Fresh " ++ show n2
    where
      f :: Int -> Int
      f a = a

showFresh :: IO ()
showFresh = do
  putStrLn "--Eff.Fresh--"
  runTrace exFresh1
  runTrace exFresh2

{- Fail & Writer.Strict -}
exFail :: (Member (Writer Int) r, Member Fail r) => Eff r Int
exFail = do
  tell (1 :: Int)
  tell (2 :: Int)
  tell (3 :: Int)
  die
  tell (4 :: Int)
  return (5 :: Int)

exFailr :: Monad m => m Int
exFailr = do
  let go :: Eff (Fail :> Writer Int :> Void) Int -> Int -- for 1.3, Fail -> Exc ()
      go = fst . run . runWriter (+) 0 . ignoreFail
      ret = go $ do
        exFail
  return ret

showFail :: IO ()
showFail = do
  putStrLn "--Eff.Fail--"
  ef <- exFailr
  putStrLn $ show ef

main :: IO ()
main = do
  showCoroutine
  showException
  showFresh
  showFail

