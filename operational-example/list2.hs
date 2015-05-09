{-# LANGUAGE GADTs #-}
import Control.Monad
import Control.Monad.Operational

data Nat n = Z | S (Nat n) deriving Show

toInt :: Nat n -> Int
toInt Z = 0
toInt (S n) = 1 + (toInt n)
  
data List s n = List {content :: [s], length' :: n} deriving Show
displayList x = show (content x, toInt $ length' x) ++ "\n"

data ListI s n a where
  Nil  :: ListI s n ()
  Cons :: s -> ListI s n ()

type ListP s n a = Program (ListI s n) a

nil = singleton Nil

cons = singleton . Cons

interpret :: ListP s (Nat n) a -> List s (Nat n) -> List s (Nat n)
interpret = eval . view
  where
    eval :: ProgramView (ListI s (Nat n)) a -> List s (Nat n) -> List s (Nat n)
    eval (Nil    :>>= is) _                                = interpret (is ()) List {content = [], length' = Z}
    eval (Cons x :>>= is) List {content = xs, length' = n} = interpret (is ()) List {content = (x:xs), length' = (S n)}
    eval (Return _      ) lst                              = lst
