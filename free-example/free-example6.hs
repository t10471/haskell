import qualified Data.IntMap as Map
import Control.Monad.Trans.Free
import Control.Monad.Trans
import Control.Monad
import Debug.Trace

newtype Ticket = TicketId {getTicketId :: Int} deriving (Eq, Ord, Show)

data Train a = Ticket (Ticket -> a) | Departure Ticket a | Station Ticket a | Skip Ticket a a

instance Functor Train where
    fmap f (Ticket g) = Ticket (f . g)
    fmap f (Departure t x) = Departure t (f x)
    fmap f (Station t x) = Station t (f x)
    fmap f (Skip t x y) = Skip t (f x) (f y)

type TimeMachineT = FreeT Train

ticket :: Monad m => TimeMachineT m Ticket
ticket = liftF (Ticket id)

departure :: Monad m => Ticket -> TimeMachineT m ()
departure t = liftF (Departure t ())

station :: Monad m => Ticket -> TimeMachineT m ()
station t = liftF (Station t ())

skippable :: Monad m => Ticket -> TimeMachineT m a -> TimeMachineT m (Maybe a)
skippable t m = FreeT $ return $ Free $ Skip t (liftM Just m) (return Nothing)

runTimeMachineT :: Monad m => TimeMachineT m a -> m a
runTimeMachineT = liftM (\(_, _, r) ->r) .run 0 Map.empty where
    run i ss m = runFreeT m >>= \r -> case r of
        Pure a                             -> return (i, ss,　a)
        Free (Ticket f)                    -> run (succ i) ss (f (TicketId i))
        Free (Departure (TicketId n) cont) -> case Map.lookup n ss of
            Just (Just m) -> run i ss m
            Nothing       -> run i (Map.insert n Nothing ss) cont
        Free (Station (TicketId n) cont)   -> case Map.lookup n ss of
            Nothing       -> run i (Map.insert n (Just cont) ss) cont
            Just _        -> run i (Map.delete n ss) cont
        Free (Skip (TicketId n) m cont)    -> case Map.lookup n ss of
            Just Nothing  -> run i ss cont
            _             -> run i ss m

ex :: TimeMachineT IO ()
ex = do
    lift $ putStrLn "begin."
    homu <- ticket
    piyo <- ticket
    station homu
    lift $ putStr "You're on Homu now. Do you want to skip to Piyo? "
    str <- lift $ getLine
    when (str == "y") $ departure piyo
    skippable piyo $ lift $ putStr "Please input:"
    str <- skippable piyo $ lift $ getLine
    lift $ print str -- スキップされた場合、strはNothingとなる
    station piyo
    lift $ putStr "You're on Piyo now. Do you want to return to Homu? "
    str <- lift $ getLine
    when (str == "y") $ departure homu
    lift $ putStrLn "end."

main :: IO ()
main = runTimeMachineT ex
