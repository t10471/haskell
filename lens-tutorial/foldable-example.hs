{-
Data.Foldableの説明

Foldableはreduceと同じ
配列を1つの値に変換する(Monoid)

以下の3つは同じこと
foldr1 (+) [1,2,3,4]
fold   [Sum 1, Sum 2, Sum 3, Sum 4]
foldMap Sum [1,2,3,4]

Data.MonoidのなかでSumが定義されている
newtype Sum a = Sum {getSum :: a} 

foldはMonoidしか取れない
fold ["hello", "world"]

foldの実装
fold :: Monoid m => t m -> m
fold xs = foldMap id xs

foldとfoldMapの関係
id　:: a -> a
fold　:: (Monoid m, Foldable t) => t m -> m
foldMap　:: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap id　:: (Monoid m, Foldable t) => t m -> m

-}
import Prelude hiding (foldr1)
import Data.Foldable
import Data.Monoid

main = do
          print $ foldr1 (+) [1,2,3,4]
          print $ getSum $ fold [Sum 1, Sum 2, Sum 3, Sum 4]
          print $ getSum $ foldMap Sum [1,2,3,4]
 
