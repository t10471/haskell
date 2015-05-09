{-# LANGUAGE StandaloneDeriving #-}

module Main where

data Variables = C Char | S String | I Int | Iex Integer | D Double | F Float
data VarList a = Var a [Variables]

deriving instance Show Variables
deriving instance Show a => Show (VarList a)

main :: IO ()
main = do
  print $ S "hello"
  print $ Iex 5
