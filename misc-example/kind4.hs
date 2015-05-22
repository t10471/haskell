{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

import GHC.TypeLits
import Data.Type.Equality
import Data.Proxy

-- TypeListsを使った型レベル数値

data Vec :: Nat -> * -> * where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (1 + n) a

-- GHC 7.6 は簡約してくれない
-- vec3 :: Vec (1 + (1 + (1 + 0))) Int

vec3 :: Vec 3 Int
vec3 = 0 `Cons` (1 `Cons` (2 `Cons` Nil))

data Foo :: Nat -> * where
  Small    :: (n <= 2) => Foo n
  Big      :: (3 <= n) => Foo n

  Empty    :: ((n == 0) ~ True)  => Foo n
  NonEmpty :: ((n == 0) ~ False) => Foo n

big :: Foo 10
big = Big

small :: Foo 2
small = Small

empty :: Foo 0
empty = Empty

nonempty :: Foo 3
nonempty = NonEmpty

-- 型レベルの等号

type Not a b = ((b == a ) ~ False)

restrictUnit :: Not () a => a -> a
restrictUnit = id

restrictChar :: Not Char a => a -> a
restrictChar = id

-- どんな型にもなる

a :: Proxy ()
a = Proxy

b :: Proxy 3
b = Proxy

c :: Proxy "symbol"
c = Proxy

d :: Proxy Maybe
d = Proxy

e :: Proxy (Maybe ())
e = Proxy

main :: IO ()
main = do
  putStrLn "end"
