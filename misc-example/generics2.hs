{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Control.Monad.Identity
import Control.Applicative

-- Data.Dataの使い方

data Animal = Cat | Dog deriving (Data, Typeable)

newtype Val = Val Int deriving (Show, Data, Typeable)

-- maybe :: b -> (a -> b) -> Maybe a -> b は 
-- Just なら関数を処理 Nothing なら第一引数を返す
incr :: Typeable a => a -> a
incr = maybe id id (cast f)
  where f (Val x) = Val (x * 100)

over :: Data a => a -> a
over x = runIdentity $ gfoldl cont base (incr x)
  where
    cont k d = k <*> (pure $ over d)
    base = pure


example1 :: Constr
example1 = toConstr Dog
-- Dog

example2 :: DataType
example2 = dataTypeOf Cat
-- DataType {tycon = "Main.Animal", datarep = AlgRep [Cat,Dog]}

example3 :: [Val]
example3 = over [Val 1, Val 2, Val 3]
-- [Val 100,Val 200,Val 300]

example4 :: (Val, Val, Val)
example4 = over (Val 1, Val 2, Val 3)
-- (Val 100,Val 200,Val 300)

numHoles :: Data a => a -> Int
numHoles = gmapQl (+) 0 (const 1)

example5 :: Int
example5 = numHoles (1,2,3,4,5,6,7)
-- 7

example6 :: Int
example6 = numHoles (Just 3)
-- 1

main :: IO ()
main = do
  putStrLn "end"
