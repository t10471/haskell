{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main ( main ) where

import MyDebug
import Control.Monad
import Control.Monad.State
import Data.Char (isUpper)
import Data.Typeable  (Typeable(..)
                      , cast
                      , typeOf
                      )
import Data.Data (Data(..)
                , gmapT
                , gmapQ
                , dataTypeOf
                , dataTypeConstrs
                , indexConstr
                , isAlgType
                , toConstr
                , fromConstr
                , fromConstrB
                , fromConstrM
                , constrType
                , constrFields
                , showConstr
                )
import Data.Generics.Aliases (extQ)
import Data.Maybe (fromJust)

data X = X { foo :: Int, bar :: Char } 
  deriving (Typeable,Data)

data Foo = Foo Int Char
  deriving (Typeable,Data, Show)

-- cast :: (Typeable a, Typeable b) => a -> Maybe b
char :: Typeable a => a -> String
char x = case cast x of       
          Just (x :: Char) -> show x
          Nothing          -> "unknown"

gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS) where
  render t
    | isTuple   = showChar '('
                . drop 1
                . commaSlots
                . showChar ')'
    | isNull    = showString "[]"
    | isList    = showChar '['
                . drop 1
                . listSlots
                . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'

    where constructor = showString . showConstr . toConstr $ t
          slots       = foldr (.) id . gmapQ        ((showChar ' ' .) . gshows) $ t
          commaSlots  = foldr (.) id . gmapQ        ((showChar ',' .) . gshows) $ t
          listSlots   = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
          isTuple     = all (==',') $ filter (not . flip elem "()") $ constructor ""
          isNull      = null        $ filter (not . flip elem "[]") $ constructor ""
          isList      = constructor "" == "(:)"

main1 :: IO()
main1 = do
  debugShow " typeOf 'a' = " (typeOf 'a')
  debugShow " typeOf 'a' == typeOf 'b' = " (typeOf 'a' == typeOf 'b')
  debugShow " typeOf 'a' == typeOf ()  = " (typeOf 'a' == typeOf ())
  debugShow " char 'a' = "   (char 'a')
  debugShow " char 5   = "   (char (5 :: Int))
  debugShow " char ()  = "   (char ())
  -- dataTypeOf :: Data a => a -> DataType
  debugShow " dataTypeOf (Just 'a') = "  (dataTypeOf (Just 'a'))
  -- dataTypeConstrs :: DataType -> [Constr]
  debugShow " dataTypeConstrs (dataTypeOf (Nothing :: Maybe ()))   = " (dataTypeConstrs (dataTypeOf (Nothing :: Maybe ())))
  debugShow " indexConstr     (dataTypeOf (Nothing :: Maybe ())) 2 = " (indexConstr (dataTypeOf (Nothing :: Maybe ())) 2)
  debugShow " isAlgType (dataTypeOf (Just 'a')) = " (isAlgType (dataTypeOf (Just 'a')))
  debugShow " isAlgType (dataTypeOf 'a')        = " (isAlgType (dataTypeOf 'a'))
  -- toConstr :: a -> Constr
  debugShow " toConstr (Just 'a') = " (toConstr (Just 'a'))
  debugShow " toConstr (Just 'a') == toConstr (Nothing :: Maybe Char) = " (toConstr (Just 'a') == toConstr (Nothing :: Maybe Char))
  debugShow " constrType (toConstr (Just 'a')) = " (constrType (toConstr (Just 'a')))
  debugShow " toConstr (X 0 'a') = " (toConstr (X 0 'a'))
  debugShow " constrFields (toConstr (X 0 'a')) = " (constrFields (toConstr (X 0 'a')))
  -- fromConstr :: Data a => Constr -> a
  debugShow " fromConstr (toConstr (Nothing :: Maybe ())) :: Maybe () = " (fromConstr (toConstr (Nothing :: Maybe ())) :: Maybe ())
  -- mConstrB :: forall a. Data a
  --         => (forall d. Data d => d) -> onstr (Just 1 :: Maybe Int)
  -- fromConstr (toConstr (1 :: Int)) :: Data a => a
  -- 型があいまいで定義できない
  -- let a = (fromConstr (toConstr (1 :: Int))) 
  let b = (toConstr (Just 1 :: Maybe Int))
  -- debugShow " fromConstrB (fromConstr (toConstr (1 :: Int))) (toConstr (Just 1 :: Maybe Int)) :: Maybe Int   = " (fromConstrB a b :: Maybe Int  )
  debugShow " fromConstrB (fromConstr (toConstr (1 :: Int))) (toConstr (Just 1 :: Maybe Int)) :: Maybe Int = " (fromConstrB (fromConstr (toConstr (1 :: Int))) b :: Maybe Int)
  -- fromConstrM :: forall m a. (Monad m, Data a)
  --            => (forall d. Data d => m d) -> Constr -> m a
  -- execState :: State s a -> s -> s
  -- execState (modify (+1)) :: Num s => s -> s
  -- execState (forM_ [1..5] (const (modify (+1)))) :: Num s => s-> s
  debugShow " execState (forM_ [1..5] (const (modify (+1)))) 5 = " (execState (forM_ [1..5] (const (modify (+1)))) 5)

main2 :: IO()
main2 = do
  let x1 = (toConstr (5::Int))
  let x2 = (toConstr 'b')
  let x3 = (toConstr (Foo 4 'a'))
  putStrLn "evalState"
  putStrLn "     (fromConstrM"
  putStrLn "       (do i <- get"
  putStrLn "           modify (+1)"
  putStrLn "           return"
  putStrLn "             (case i of"
  putStrLn "               0 -> fromConstr (toConstr (5::Int))"
  putStrLn "               1 -> fromConstr (toConstr 'b')))"
  putStrLn "       (toConstr (Foo 4 'a')))"
  putStrLn "     0 :: Foo"
  print (evalState (fromConstrM (do i <- get; modify ((+1) :: Int -> Int); return (case i of; 0 -> fromConstr x1; 1 -> fromConstr x2)) x3) 0 :: Foo)
  -- gmapT :: forall a. Data a
  --       => (forall b. Data b => b -> b) -> a -> a
  putStrLn "gmapT"
  putStrLn "     (\\d ->"
  putStrLn "        case cast d of"
  putStrLn "          Nothing -> d"
  putStrLn "          Just x ->"
  putStrLn "            fromJust (cast (if isUpper x then '!' else x)))"
  putStrLn "     (Foo 4 'a')"
  print $ gmapT (\d -> case cast d of; Nothing -> d ;Just x -> fromJust (cast (if isUpper x then '!' else x))) (Foo 4 'a')
  putStrLn "gmapT"
  putStrLn "     (\\d ->"
  putStrLn "        case cast d of"
  putStrLn "          Nothing -> d"
  putStrLn "          Just x ->"
  putStrLn "            fromJust (cast (if isUpper x then '!' else x)))"
  putStrLn "     (Foo 4 'A')"
  print $ gmapT (\d -> case cast d of; Nothing -> d ;Just x -> fromJust (cast (if isUpper x then '!' else x))) (Foo 4 'A')

main3 :: IO()
main3 = do
  -- gmapQ :: forall a. Data a => (forall d. Data d => d -> u) -> a -> [u]
  debugShow " gmapQ (\\d -> toConstr d) (Foo 5 'a') = " (gmapQ (\d -> toConstr d) (Foo 5 'a'))
  putStrLn "gshows ([Just (2 :: Int)],'c', Foo 5'a') = "
  print $ (gshows ([Just (2 :: Int)],'c', Foo 5'a')) ""
  print "end"

main :: IO()
main = do
  main1
  main2
  main3
