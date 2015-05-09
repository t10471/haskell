{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}

-- 関数のオーバーロード
class Foo a where
    foo :: a -> String

instance (Num a, Eq a) => Foo a where
    foo 1 = "bar"
    foo _ = "?"

instance Foo String where
    foo "1" = "baz"
    foo _   = "?"

-- IncoherentInstances のサンプル
class ToString a where
    toString :: a -> String

newtype Wrap a = Wrap a

instance Show a => ToString (Wrap a) where
    toString (Wrap x) = "Normal:" ++ show x

instance ToString (Wrap Int) where
    toString (Wrap x) = "Int:" ++ show x

f :: (Show a, Num a) => Wrap a -> String
f = toString

main :: IO ()
main = do
  putStrLn $ foo 1
  putStrLn $ foo "1"
  print $ toString (Wrap 1)
  -- "Normal:1"
  print $ toString (Wrap (1 :: Int ))
  -- "Int:1"
  print $ f (Wrap (1 :: Int ))
  -- "Normal:1"
  print $ toString (Wrap (1 :: Int ))
  -- "Int:1"
