module Main  where

y :: (a -> a ) -> a
y x = x (y x )

-- fact :: Num a => (a -> a ) -> a -> a
fact :: (Eq a, Num a) => (a -> a ) -> a -> a
fact = \f n -> if n == 0 then 1 else n * f (n-1)

main :: IO()
main  = do
  print "main"
