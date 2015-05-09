{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

import GHC.TypeLits(  Nat      -- 自然数型
                    , KnownNat -- natVal を使うための制約
                    , CmpNat   -- 比較できる自然数型
                    , natVal   -- 自然数型をIntegerにする
                    , type(-)) -- 自然数型の引き算
import Data.Type.Equality(type(==))
import Data.Proxy(Proxy(..))

{-
Proxy の使い方
ProxyはPolyKindsで
Proxy :: k -> * となっている。
kは多相カインドである。 (* -> *や* -> * -> *など)
Proxyはどんなカインドも*としてあつかえる

undefinedはkindが*だけなので
tzero = undefined :: 'TZero
はエラーになるがProxyは定義できる
data TNat = TZero | TSucc TNat
tzero = undefined :: 'TZero
tzero = Proxy :: Proxy 'TZero
TNat :: *
TZero :: TNat

-}

type family (a :: Nat) % (b :: Nat) :: Nat where
  a % b = Mod a b (CmpNat a b)
type family Mod (a :: Nat) (b :: Nat) (k :: Ordering) :: Nat where
  Mod a b EQ = 0
  Mod a b LT = a
  Mod a b GT = Mod (a-b) b (CmpNat (a-b) b)

data FizzBuzzFlag = FizzBuzz | Fizz | Buzz | Natural Nat

class ShowFB (f :: FizzBuzzFlag) where
  showFB :: Proxy f -> String

instance ShowFB FizzBuzz where showFB _ = "FizzBuzz"
instance ShowFB Fizz     where showFB _ = "Fizz"
instance ShowFB Buzz     where showFB _ = "Buzz"
instance (KnownNat n) => ShowFB (Natural n) where 
  showFB _ = show $ natVal $ (Proxy :: Proxy n)

type family ToFizzBuzz (a :: Nat) :: FizzBuzzFlag where
  ToFizzBuzz a = ToFizzBuzz' a (a % 3) (a % 5)
type family ToFizzBuzz' (a :: Nat) (b3 :: Nat) (b5 :: Nat) :: FizzBuzzFlag where
  ToFizzBuzz' a 0 0   = FizzBuzz
  ToFizzBuzz' a 0 b5  = Fizz
  ToFizzBuzz' a b3 0  = Buzz
  ToFizzBuzz' a b3 b5 = Natural a

fizzbuzz :: (ShowFB (ToFizzBuzz n)) => Proxy (n :: Nat) -> String
fizzbuzz (Proxy :: Proxy m) = showFB $ (Proxy :: Proxy (ToFizzBuzz m))

class FizzBuzzList' (b :: Bool) (n :: Nat) where
  fizzbuzzList' :: Proxy b -> Proxy n -> [String] -> [String]

instance FizzBuzzList' True 0 where
  fizzbuzzList' _ _ acc = acc
instance (ShowFB (ToFizzBuzz n), FizzBuzzList' (n == 1) (n-1)) => FizzBuzzList' False n where
  fizzbuzzList' _ k@(Proxy :: Proxy n) acc = fizzbuzzList' (Proxy :: Proxy (n == 1)) (Proxy :: Proxy (n-1)) ((fizzbuzz k) : acc)

fizzbuzzList :: (FizzBuzzList' (n==0) n) => Proxy (n :: Nat) -> [String]
fizzbuzzList x@(Proxy :: Proxy n) = fizzbuzzList' (Proxy :: Proxy (n==0)) x []

main = do
  mapM_ putStrLn $ fizzbuzzList (Proxy :: Proxy 16)
