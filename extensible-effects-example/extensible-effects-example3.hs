{-#LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable, TypeOperators#-}

import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)

import Control.Eff
import Control.Eff.State.Strict
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.Exception
import Control.Eff.Coroutine
import Control.Eff.Choose
import Control.Eff.Cut

import Data.Typeable
import Data.Void

-- | Choose: 非決定性計算 Listモナドと同等？
chooseTest :: Eff (Choose :> r) (Int, Int)
chooseTest = (,) <$> choose [0..4] <*> choose [5..9]

--------------------------------------------------------------------------------

-- | Coroutine: yieldで中断可能な計算？
yieldTest :: Eff (Yield Int :> Lift IO :> ()) Int
yieldTest = do
  yield (1::Int)
  lift $ threadDelay (10^6)
  yield (2::Int)
  lift $ threadDelay (10^6)
  yield (3::Int)
  lift $ threadDelay (10^6)
  return 4

-- | 終了したらその値を返し、Yieldが来たらYieldされた値をprint、最後まで繰り返す。
yieldRunnerIO :: (Show a, Typeable a) =>
                 Eff (Yield a :> Lift IO :> ()) b -> IO b
yieldRunnerIO m = runLift (runC m) >>= go
  where go (Done w) = return w
        go (Y a c)  = putStrLn ("Y: " ++ show a) >> runLift (c ()) >>= go

-- | 終了したらその値を返し、Yieldが来たらYieldされた値をprint、偶数ならprintして中断する。
yieldRunnerIO' ::  Eff (Yield Int :> Lift IO :> ()) Int -> IO Int
yieldRunnerIO' m = runLift (runC m) >>= go
  where go (Done w) = return w
        go (Y a c) | a `mod` 2 == 0 = putStrLn ("Interupt: " ++ show a) >> return a
                   | otherwise  = putStrLn ("Y: " ++ show a) >> runLift (c ()) >>= go


--------------------------------------------------------------------------------

data FooException = FooException deriving(Show, Typeable)
data BarException = BarException deriving(Show, Typeable)

-- | Exc: 例外。投げられる例外を型で制限できる。
excTest1 :: Eff (Exc FooException :> r) Int
excTest1 = do
  throwExc FooException
  return 1

-- 型エラーで弾かれる
{-
excTest1' :: Eff (Exc FooException :> r) Int
excTest1' = do
  throwExc BarException
  return 1
-}
  
excTest2 :: Eff (Exc FooException :> Exc BarException :> r) Int
excTest2 = return 1

-- | runExcしたので、型からFooExceptionが消える。
excTest3 :: Eff (Exc BarException :> Lift IO :> ()) Int
excTest3 = do
  -- Foo のエラー処理をするイメージ
  runExc excTest1 >>= \r -> case r of
    Left FooException -> lift (putStrLn "Foo Exception occured.")
    Right i           -> lift $ print i
    
  return 10

--------------------------------------------------------------------------------
-- | Fresh: 呼び出す毎にsuccされる。カウンター用？
freshTest :: Eff (Fresh Int :> Void) [Int]
freshTest = replicateM 10 fresh

--------------------------------------------------------------------------------
-- | 状態モナド。複数存在しても型で勝手に選んでくれる。
stateTest :: Eff (State Double :> State Int :> Void) Double
stateTest = do
  a <- get
  b <- get
  put (a*5)
  put (b*5)
  modify (succ :: Double -> Double)
  return $ (a::Double) ^ (b::Int)

main :: IO ()
main = do
  putStrLn "Choose: "
  print $ run $ runChoice chooseTest
  putStrLn "Coroutine: "
  yieldRunnerIO yieldTest
  putStrLn "Coroutine': "
  yieldRunnerIO' yieldTest
  putStrLn "Exception: "
  print . run . runExc $ excTest1
  print . run . runExc . runExc $ excTest2
  runLift (runExc excTest3) >>= print
  putStrLn "Fail: "
  print $ (run $ runFail die :: Maybe Int) 
  print . run $ runFail (return 1)
  putStrLn "Fresh: "
  print . run $ runFresh freshTest 1
  print . run $ runFresh freshTest 6
  putStrLn "State: "

  -- run* は左から順にrunするのでここに型注釈は不要
  print . run . runState 5 {- :: Int-} $ runState 10 {- :: Double-} stateTest 


-- Choose: 
-- [(0,5),(0,6),(0,7),(0,8),(0,9),(1,5),(1,6),(1,7),(1,8),(1,9),(2,5),(2,6),(2,7),(2,8),(2,9),(3,5),(3,6),(3,7),(3,8),(3,9),(4,5),(4,6),(4,7),(4,8),(4,9)]
-- Coroutine: 
-- Y: 1                  <- 1sスリープ
-- Y: 2                  <- 1sスリープ
-- Y: 3                  <- 1sスリープ
-- Coroutine': 
-- Y: 1                  <- 1sスリープ
-- Interupt: 2           <- スリープしない
-- Exception: 
-- Left FooException
-- Right (Right 1)
-- Foo Exception occured.
-- Right 10
-- Fail: 
-- Nothing
-- Just 1
-- Fresh: 
-- [1,2,3,4,5,6,7,8,9,10]
-- [6,7,8,9,10,11,12,13,14,15]
-- State: 
-- (25,(51.0,100000.0))



