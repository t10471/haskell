{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs        #-}
import qualified Data.Sequence as Seq

-- TypeFamiliesとGADTsの比較
-- add 関数はどちらでも実装できるが、
-- 引数の無いemptyはGADTでは実装することができない
-- firstも実装できない
-- extentions-example8.hsでMultiParamTypeClasses
-- を使ったサンプルにも似た問題があり

class VectorElem a where
  data Vector a
  add :: a -> Vector a -> Vector a
  first :: Vector a -> a
  empty :: Vector a

instance VectorElem Bool where
  data Vector Bool = BoolVector [Bool]
  add v (BoolVector l) = BoolVector $ v:l
  first (BoolVector l) = head l
  empty = BoolVector []

instance VectorElem Int where
  data Vector Int = IntVector (Seq.Seq Int)
  add v (IntVector s) = IntVector $ v Seq.<| s
  first (IntVector s) = Seq.index s 0
  empty = IntVector Seq.empty

data VectorA a where
  BoolVectorA :: [Bool]      -> VectorA Bool
  IntVectorA  :: Seq.Seq Int -> VectorA Int

addA :: a -> VectorA a -> VectorA a
addA v (BoolVectorA l) = BoolVectorA $ v:l
addA v (IntVectorA s)  = IntVectorA  $ v Seq.<| s

-- 型別に関数を定義しなければいけない
firstB :: VectorA Bool -> Bool
firstB (BoolVectorA l) = head l

firstI :: VectorA Int -> Int
firstI (IntVectorA s)  = Seq.index s 0

emptyB = BoolVectorA []
emptyI = IntVectorA Seq.empty

main :: IO ()
main = do
  print $ first $ add True empty
  print $ (first $ add 1 empty :: Int)
  print $ firstB $ addA False emptyB
  print $ firstI $ addA 1 emptyI
  putStrLn "end"
