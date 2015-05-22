{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import Data.Dynamic
import Data.Maybe

data Animal = Cat | Dog deriving (Show, Typeable)
data Zoo a = Zoo [a] deriving Typeable

equal :: (Typeable a, Typeable b) => a -> b -> Bool
equal a b = typeOf a == typeOf b

example1 :: TypeRep
example1 = typeOf Cat
-- Animal

example2 :: TypeRep
example2 = typeOf (Zoo [Cat, Dog])
-- Zoo Animal

example3 :: TypeRep
example3 = typeOf ((1, 6.636e-34, "foo") :: (Int, Double, String))
-- (Int,Double,[Char])

example4 :: Bool
example4 = equal False ()

-- cast は型がその型がどうかを確認する感じ？
exCast1 :: Maybe Animal
exCast1 = cast Cat

exCast2 :: Maybe Animal
exCast2 = cast (1 :: Int)

-- 動的キャスト

dynamicBox :: Dynamic
dynamicBox = toDyn (6.62 :: Double)

example5 :: Maybe Int
example5 = fromDynamic dynamicBox
-- Nothing

example6 :: Maybe Double
example6 = fromDynamic dynamicBox
-- Just 6.62

example7 :: Int
example7 = fromDyn dynamicBox 0
-- 0

example8 :: Double
example8 = fromDyn dynamicBox 0.0
-- 6.62

-- Animalにderiving Dataをつけた時の中身?

instance Data Animal where
  gfoldl k z Cat = z Cat
  gfoldl k z Dog = z Dog

  gunfold k z c
    = case constrIndex c of
        1 -> z Cat
        2 -> z Dog

  toConstr Cat = cCat
  toConstr Dog = cDog

  dataTypeOf _ = tAnimal

tAnimal :: DataType
tAnimal = mkDataType "Main.Animal" [cCat, cDog]

cCat :: Constr
cCat = mkConstr tAnimal "Cat" [] Prefix

cDog :: Constr
cDog = mkConstr tAnimal "Dog" [] Prefix

-- [] のDataの中身?

instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl k z (x:xs) = z (:) `k` x `k` xs

  toConstr []    = nilConstr
  toConstr (_:_) = consConstr

  gunfold k z c
    = case constrIndex c of
        1 -> z []
        2 -> k (k (z (:)))

  dataTypeOf _ = listDataType

nilConstr :: Constr
nilConstr = mkConstr listDataType "[]" [] Prefix

consConstr :: Constr
consConstr = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]

-- TapleのDataの中身?

instance (Data a, Data b) => Data (a,b) where
  gfoldl k z (a,b) = z (,) `k` a `k` b

  toConstr (_,_) = tuple2Constr

  gunfold k z c
    = case constrIndex c of
      1 -> k (k (z (,)))

  dataTypeOf _  = tuple2DataType

tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]


main = do
  putStrLn "end"
