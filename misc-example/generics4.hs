{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Hashable
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Aeson as A

-- ハッシュ関数を導出する

data Color = Red | Green | Blue deriving (Generic, Show)

instance Hashable Color where

example1 :: Int
example1 = hash Red
-- 839657738087498284

example2 :: Int
example2 = hashWithSalt 0xDEADBEEF Red

-- バイナリの自動導出

data Val = A [Val] | B [(Val, Val)] | C
  deriving (Generic, Show)

instance S.Serialize Val where

encoded :: B.ByteString
encoded = S.encode (A [B [(C, C)]])
-- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\STX"

bytes :: [Word8]
bytes = B.unpack encoded
-- [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,2,2]

decoded :: Either String Val
decoded = S.decode encoded

-- jsonを導出する

data Point = Point { _x :: Double, _y :: Double }
   deriving (Show, Generic)

instance A.FromJSON Point
instance A.ToJSON Point

example3 :: Maybe Point
example3 = A.decode "{\"x\":3.0,\"y\":-1.0}"

example4 = A.encode $ Point 123.4 20

main :: IO ()
main = do
  putStrLn "end"
