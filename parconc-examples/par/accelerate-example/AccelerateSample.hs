
{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

module Main ( main ) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import MyDebug


main :: IO ()
main = do
  let a = fromList (Z:.10) [1..10] :: Vector Int
  let b = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
  debugShow " a = fromList (Z:.10) [1..10] :: Vector Int = " (a)
  debugShow " b = fromList (Z:.3:.5) [1..] :: DIM2 Int = " (b)
  -- Accelerate 外でのインデックスの取得
  debugShow " indexArray b (Z:.2:.1) = " (indexArray b (Z:.2:.1))
  -- 計算は Acc の中で行うため use と run が必要
  -- run :: Acc (Array DIM2 Int) -> Array DIM2 Int 
  -- use :: Array DIM2 Int -> Acc (Array DIM2 Int)
  debugShow " run $ A.map (+1) (use b) = " (run $ A.map (+1) (use b))
  let s = unit (3 :: Exp Int)
  -- unit でスカラを取り出す
  debugShow " run $ unit (3 :: Exp Int) = " (run s)
  -- the で Exp e を取り出す Exp は単一の値 Accは配列
  debugShow " the s :: Exp Int = " (the s :: Exp Int)
  -- ! Acc (Array DIM1 Int) -> Exp DIM1 -> Exp Int
  -- index1 3 => Exp Z:.3
  -- index1 Exp Int -> Exp (Z :. Int )
  debugShow " run $ unit (use a ! indx1 3) = " (run $ unit (use a ! index1 3))
  -- 配列の作成
  debugShow " run $ fill (index2 3 5) 4 :: Array DIM2 Int = " (run $ fill (index2 3 5) 4 :: Array DIM2 Int)
  debugShow " run $ enumFromN (index2 3 5) 4 :: Array DIM2 Int = " (run $ enumFromN (index2 3 5) 4 :: Array DIM2 Int)
  debugShow " run $ enumFromStepN (index2 3 5) 4 4 :: Array DIM2 Int = " (run $ enumFromStepN (index2 3 5) 4 4 :: Array DIM2 Int)
  debugShow " (run $ generate (index2 3 5) (ix -> let Z:.x:.y = unlift ix in x + y)) :: Array DIM2 Int = " ((run $ generate (index2 3 5) (\ix -> let Z:.x:.y = unlift ix in x + y)) :: Array DIM2 Int)
  -- =>  Array DIM2 Int = Array (Z :. 3 :. 5 ) [0,1,2,3,4,1,2,3,4,5,2,3,4,5,6]
  let a1 = enumFromN (index2 2 3) 1 :: Acc (Array DIM2 Int)
  let a2 = enumFromStepN (index2 2 3) 6 (-1) :: Acc (Array DIM2 Int)
  debugShow " run $ A.zipWith (+) a1 a2 = " (run $ A.zipWith (+) a1 a2)
  let b1 = enumFromN (index2 2 3) 1 :: Acc (Array DIM2 Int)
  let b2 = enumFromStepN (index2 3 5) 10 10 :: Acc (Array DIM2 Int)
  -- 違うシェイプの場合
  debugShow " run $ A.zipWith (+) b1 b2 = " (run $ A.zipWith (+) b1 b2)


  print "end"
