import Prelude hiding (break)
import Control.Monad.Trans.Free
import Control.Monad.Trans (lift)
import Control.Monad(when)

data Zero a = Zero deriving (Show, Eq, Ord)
instance Functor Zero where
    fmap _ _ = Zero

type BreakT = FreeT Zero

break :: Monad m => BreakT m ()
break = liftF Zero

runBreakT :: Monad m => BreakT m a -> m ()
runBreakT m = runFreeT m >>= \r -> case r of
    Free Zero -> return ()

ex :: Int -> BreakT IO ()
ex n = do
    lift $ putStr "Can you break me? :"
    str <- lift getLine
    when (str == "exit")
        $ lift (putStrLn "Exactly!") >> break
    ex (succ n)

main :: IO ()
main = do
  runBreakT (ex 0)
  putStrLn "end"
