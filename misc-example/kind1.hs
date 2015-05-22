{-# LANGUAGE PolyKinds #-}

-- どんな型にもなる
data Proxy a = Proxy
data Rep = Rep

class PolyClass a where
  foo :: Proxy a -> Rep
  foo = const Rep

-- () :: *
-- [] :: * -> *
-- Either :: * -> * -> *

instance PolyClass ()
instance PolyClass []
instance PolyClass Either

-- いろんなカインドを定義
newtype I (a :: *) = I a
newtype K (a :: *) (b :: k) = K a
newtype Flip (f :: k1 -> k2 -> *) (x :: k2) (y :: k1) = Flip (f y x)

unI :: I a -> a
unI (I x) = x

unK :: K a b -> a
unK (K x) = x

unFlip :: Flip f x y -> f y x
unFlip (Flip x) = x


e :: Either String Int
e = Right 1

-- f :: Flip Either Int String
f = Flip e

main :: IO ()
main = do
  print e
  putStrLn "end"
