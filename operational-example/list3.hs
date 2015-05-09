{-# LANGUAGE GADTs #-}
import Control.Monad
import Control.Monad.Operational
import Control.Applicative
import Control.Monad.Trans

data Nat n = Z | S (Nat n) deriving Show

toInt :: Nat n -> Int
toInt Z = 0
toInt (S n) = 1 + (toInt n)
  
data List s n = List {content :: [s], length' :: n} deriving Show
displayList x = show (content x, toInt $ length' x) ++ "\n"

data ListI s n a where
  Nil  :: ListI s n ()
  Cons :: s -> ListI s n ()

type ListP s n m a = ProgramT (ListI s n) m a

nil :: (Monad m) => ListP s n m ()
nil = singleton Nil

cons :: (Monad m) => s -> ListP s n m ()
cons = singleton . Cons

interpret :: (Monad m) => ListP s (Nat n) m a -> List s (Nat n) -> m (List s (Nat n))
interpret is list = (\v -> eval v list) =<< (viewT is)
  where
    eval :: (Monad m) => ProgramViewT (ListI s (Nat n)) m a -> List s (Nat n) -> m (List s (Nat n))
    eval (Nil    :>>= is) _                                = interpret (is ()) List {content = [], length' = Z}
    eval (Cons x :>>= is) List {content = xs, length' = n} = interpret (is ()) List {content = (x:xs), length' = (S n)}
    eval (Return _      ) list                             = return list

makeList :: ListP Char n IO ()
makeList = do
  nil
  (liftIO . putStrLn) "Input an element."
  cons =<< read <$> liftIO getLine
  (liftIO . putStrLn) "Input an element."
  cons =<< read <$> liftIO getLine
  (liftIO . putStrLn) "Input an element."
  cons =<< read <$> liftIO getLine

main :: IO ()
main = do
  (putStrLn . displayList) =<< interpret makeList (List {content = [], length' = Z})
