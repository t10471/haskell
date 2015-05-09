{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

-- TypeFamiliesとFunctionalDependenciesのサンプルと
-- 等式制約を使ったパターン


class Mul a b where
  type Result a b
  mul :: a -> b -> Result a b

instance Mul Int Int where
  type Result Int Int = Int
  mul = (Prelude.*)

instance Mul Int Double where
  type Result Int Double = Double
  mul x y = fromIntegral x Prelude.* y

class MulA a b c | a b -> c where
  mulA :: a -> b -> c

instance MulA Int Int Int where
  mulA = (Prelude.*)

instance MulA Int Double Double where
  mulA x y = fromIntegral x Prelude.* y

-- 付けるメリットが分からない・・・

class (ResultB a b ~ c) => MulB a b c where
  type ResultB a b
  mulB :: a -> b -> c

instance MulB Int Int Int where
  type ResultB Int Int = Int
  mulB = (Prelude.*)

instance MulB Int Double Double where
  type ResultB Int Double = Double
  mulB x y = fromIntegral x Prelude.* y

main :: IO ()
main = do
  print $ mul  (1 :: Int) (2 :: Int)
  print $ mul  (1 :: Int) (2.0 :: Double)
  print $ mulA (1 :: Int) (2 :: Int)
  print $ mulA (1 :: Int) (2.0 :: Double)
  print $ mulB (1 :: Int) (2 :: Int)
  print $ mulB (1 :: Int) (2.0 :: Double)
  putStrLn "end"
