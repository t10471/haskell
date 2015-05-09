{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- module Transformers where
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map

type Env    = Map.Map Int String 
type OpT = StateT [Integer] Maybe Integer
type ReT = ReaderT [Integer] Maybe Integer

pushT :: Integer -> OpT
pushT a = do
    as <- get
    put (a:as)
    return a

popT :: OpT
popT = do
    as <- get
    f as
  where
    f []     = lift Nothing
    f (a:as) = do
        put as
        return a

-- type EvalR a = ReaderT Env (ExceptT String Identity) a
-- runEvalR :: Env -> EvalR a -> Either String a
-- runEvalR env ev = runIdentity (runExceptT (runReaderT ev env))

printReaderContent :: ReaderT String IO ()
printReaderContent = do
    content <- ask
    liftIO $ putStrLn ("The Reader Content: " ++ content)

exReaderT = do
  runReaderT printReaderContent "Some Content"

type Bindings = Map.Map String Int;

isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
    count <- asks (lookupVar "count")
    bindings <- ask
    return (count == (Map.size bindings))

lookupVar :: String -> Bindings -> Int
lookupVar name bindings = fromJust (Map.lookup name bindings)

sampleBindings = Map.fromList [("count",3), ("1",1), ("b",2)]

exReader = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": ";
    putStrLn $ show (isCountCorrect sampleBindings);

type MyAppEnv = Env
type MyAppState = Integer

newtype MyAppT m a = MyAppT { runMyAppT :: ReaderT MyAppEnv (StateT MyAppState m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runMyApp :: Monad m => MyAppT m a -> MyAppState -> MyAppEnv -> m a
runMyApp ap st env = do
  (a, s) <- runStateT (runReaderT (runMyAppT ap) env) st
  return a

app :: MonadIO m => MyAppT m ()
app = do
    liftIO $ putStrLn "Hello, MyApp!"



main :: IO ()
main = do
  let stack = [1..5]
  print $ flip runStateT stack $ do
    popT
    popT
  exReaderT
  runMyApp app 0 Map.empty
