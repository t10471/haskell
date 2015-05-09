-- module MyMaybeT (
--     MyMaybeT(..)
--   , isValid
--   , getValidPassword
--   , askPassword
--   , askPassword'
-- ) where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.Char

newtype MyMaybeT m a = MyMaybeT {runMyMaybeT :: m (Maybe a)}

instance Monad m => Monad (MyMaybeT m) where
    return = MyMaybeT . return . Just
    x >>= f = MyMaybeT $ do
        maybe_value <- runMyMaybeT x
        case maybe_value of
              Nothing -> return Nothing
              Just value -> runMyMaybeT $ f value

instance Monad m => MonadPlus (MyMaybeT m) where
    mzero = MyMaybeT $ return Nothing
    mplus x y = MyMaybeT $ do
        maybe_value <- runMyMaybeT x
        case maybe_value of
            Nothing -> runMyMaybeT y
            Just value -> runMyMaybeT x

instance MonadTrans MyMaybeT where
    lift = MyMaybeT . (liftM Just)

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

getValidPassword :: MyMaybeT IO String
getValidPassword = do
    s <- lift getLine
    guard (isValid s)
    return s

askPassword :: MyMaybeT IO ()
askPassword = do
    lift $ putStrLn "Insert your new password: "
    value <- getValidPassword
    lift $ putStrLn "Storing in database ... "

askPassword' :: MyMaybeT IO ()
askPassword' = do
    lift $ putStrLn "Insert your new password: "
    value <- msum $ repeat getValidPassword
    lift $ putStrLn "Storing in database ... "

main :: IO ()
main = do
  runMyMaybeT askPassword
  return ()
