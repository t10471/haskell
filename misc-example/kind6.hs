{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Proxy
import GHC.TypeLits

{-
someNatVal :: Integer -> Maybe SomeNat
someSymbolVal :: String -> SomeSymbol

natVal :: KnownNat n => proxy n -> Integer
symbolVal :: KnownSymbol n => proxy n -> String
-}


a :: Integer
a = natVal (Proxy :: Proxy 1)
-- 1

b :: String
b = symbolVal (Proxy :: Proxy "foo")
-- "foo"

c :: Integer
c = natVal (Proxy :: Proxy (2 + 3))
-- 5

main :: IO ()
main = do
  putStrLn "end"
