{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-}

module Main ( main ) where

import Data.Array.Repa as R
import Data.Functor.Identity
import MyDebug

main :: IO ()
main = do
  -- Repa の使い方

  let a = fromListUnboxed (Z:.10) [1..10] :: Array U DIM1 Int
  debugShow " fromListUnboxed (Z:.10) [1..10] :: Array U DIM1 Int = " (a)
  debugShow " toIndex (Z:.3:.5 :: DIM2) (Z:.2:.1 :: DIM2) = " (toIndex (Z:.3:.5 :: DIM2) (Z:.2:.1 :: DIM2))
  -- => 11
  let arr1 = fromListUnboxed (Z:.3:.5) [1..15] :: Array U DIM2 Int
  debugShow " arr1 ! (Z:.2:.1) = " (arr1 ! (Z:.2:.1))
  -- => 12
  debugShow " rank $ extent arr1 = " (rank $ extent arr1)
  -- => 2
  debugShow " size $ extent arr1 = " (size $ extent arr1)
  -- => 5
  let arr2 = reshape (Z:.5:.3 :: DIM2) arr1
  debugShow " arr2 ! (Z:.2:.1) = " (arr2 ! (Z:.2:.1))
  -- => 8 
  debugShow " rank $ extent arr2 = " (rank $ extent arr2)
  -- => 2
  debugShow " size $ extent arr2 = " (size $ extent arr2)
  -- => 5
  -- reshapeしてもsizeとrankは変わらない

  debugShow " computeS (R.map (+1) a) = " (computeS (R.map (+1) a) :: Array U DIM1 Int)
  debugShow " computeS (R.map (+1) (R.map (^2) a)) = " (computeS (R.map (+1) (R.map (^2) a)) :: Array U DIM1 Int)
  let f = fromFunction (Z:.10) (\(Z:.i) -> i :: Int)
  debugShow " fromFunction (Z:.10) (\\(Z:.i) -> i :: Int) ! (Z:.5) = " (f ! (Z:.5)) 
  let mymap f a = fromFunction (extent a) (\ix -> f (a ! ix))
  let c = mymap (+1) a
  debugShow " computeS (mymap (+1) a) = " (computeS c :: Array U DIM1 Int)
  -- Monad ならなんでもいいので Identity を使用
  debugShow " computeP (mymap (+1) a) = " ((runIdentity (computeP c)) :: Array U DIM1 Int)

  -- computeP の連続適用
  let arr3 = fromFunction (Z:.5) (\(Z:.i) -> i :: Int)
  parr <- computeP arr3 :: IO(Array U DIM1 Int)
  let arr4 = fromFunction (Z:.5) (\ix -> parr ! ix)
  debugShow " nest computeP " ((runIdentity (computeP arr4)) :: Array U DIM1 Int)
  debugShow " sumAllS arr1 = " (sumAllS arr1)
  -- => 120
  debugShow " foldS (+) 0 arr1 = " (foldS (+) 0 arr1)
  -- => AUnboxed (Z :. 3) (fromList [15,40,65])
  debugShow " foldP (+) 0 arr1 = " (runIdentity(foldP (+) 0 arr1))
  print "end"
