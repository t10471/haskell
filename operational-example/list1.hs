{-# LANGUAGE GADTs #-}
import Control.Monad
import Control.Monad.Operational

data List s n = List {content :: [s], length' :: Int} deriving Show
displayList x = show (content x, length' x) ++ "\n"

data ListI s n a where
  Nil  :: ListI s n ()
  Cons :: s -> ListI s n ()

type ListP s n a = Program (ListI s n) a

nil = singleton Nil

cons = singleton . Cons

interpret :: ListP s n a -> List s n -> List s n
interpret = eval . view
  where
    eval :: ProgramView (ListI s n) a -> List s n -> List s n
    eval (Nil    :>>= is) _                                = interpret (is ()) List {content = [], length' = 0}
    eval (Cons x :>>= is) List {content = xs, length' = n} = interpret (is ()) List {content = (x:xs), length' = ((+1) n)}
    eval (Return _      ) lst                              = lst
