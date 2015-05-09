{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}

import Data.Maybe (mapMaybe)

-- 証拠
data Wit :: * -> * where
  IntWit :: Wit Int -- Int型である
  BoolWit :: Wit Bool -- Bool型である
  StringWit :: Wit String -- String型である
  ShowWit :: Show a => Wit a -- Showクラスに属する何かである…！

-- 証拠つきの存在型
data Ex = forall a. Ex (Wit a) a

-- 適当なデータ型 (Showクラスに属する)
data MyData = MyData deriving Show


heterolist :: [Ex]
heterolist = [Ex IntWit 1, Ex BoolWit True, Ex StringWit "abc", Ex ShowWit MyData]

--  GADTが証拠として機能するため, 個々の型のshowが使える
instance Show Ex where
  show (Ex IntWit i)      = show i ++ "::Int"
  show (Ex BoolWit b)     = show b ++ "::Bool"
  show (Ex StringWit str) = show str ++ "::String"
  show (Ex ShowWit a)     = show a ++"::Show a=>a"

fromInt:: Ex -> Maybe Int
fromInt (Ex IntWit i) = Just i
fromInt _             = Nothing

extractInt = mapMaybe fromInt

main :: IO ()
main = do
  print heterolist
  print $ extractInt heterolist
