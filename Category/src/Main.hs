
module Main where

import Magma
import Data.Monoid


main = do
  let tree1 = (Leaf 0 `magappend` Leaf 1) `magappend` Leaf 2
  let tree2 = Leaf 0 `magappend` (Leaf 1 `magappend` Leaf 2)
  -- 自由マグマでは演算順序が区別されます。
  print $ foldMapMagma (Kakko . show) tree1
  -- Kakko "((0 1) 2)"
  print $ foldMapMagma (Kakko . show) tree2
  -- Kakko "(0 (1 2))"
  
  -- モノイドの演算をそのまま使うことも出来ます。
  print $ foldMapMagma (WrapMonoid . Sum) tree1
  -- WrapMonoid (Sum {getSum = 3})
  print $ foldMapMagma (WrapMonoid . All . even) tree1
  -- WrapMonoid (All {getAll = False})
