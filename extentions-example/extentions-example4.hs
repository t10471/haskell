{-# LANGUAGE ScopedTypeVariables #-}
 
-- ScopedTypeVariables をつけることにより
-- 局所変数にも同じ型を使用することができる

bounds :: forall a. (Show a, Bounded a) => a -> (String, String)
bounds x = (minBoundS, maxBoundS)
  where
    -- a が上の定義と同じになるのは forall のおかげ
    minBoundS = show (minBound :: a)
    maxBoundS = show (maxBound :: a)
 
-- forall を使えない場合の回避策
bounds' :: (Show a, Bounded a) => a -> (String, String)
bounds' x = (minBoundS, maxBoundS)
  where
    minBoundS = show (minBound `asTypeOf` x)
    maxBoundS = show (maxBound `asTypeOf` x)

main :: IO ()
main = do
  print $ bounds (3 :: Int)
