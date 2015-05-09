import Data.Monoid
import Control.Monad.Trans.Free

type ConveyorT a = FreeT ((->) a)
type GeneratorT a = FreeT ((,) a)

ask :: Monad m => ConveyorT r m r 
ask = liftF id

tell :: Monad m => w -> GeneratorT w m ()
tell a = liftF (a, ())

-- xs は同じ値の無限リスト
runConveyorT :: Monad m => ConveyorT r m a -> [r] -> m a
runConveyorT m xs = runFreeT m >>= \r -> case r of
    Pure a -> return a
    Free f -> runConveyorT (f (head xs)) (tail xs)

runGeneratorT :: Monad m => GeneratorT w m a -> m (a, [w])
runGeneratorT m = runFreeT m >>= \r -> case r of
    Pure a -> return (a, [])
    Free (x, cont) -> do
        (a, xs) <- runGeneratorT cont
        return (a, x : xs)

type ReaderT a = ConveyorT a
type WriterT a = GeneratorT a

-- repeatを使って無限リストにして同じ値を何度も取得できるようにしている
runReaderT :: Monad m => ReaderT r m a -> r -> m a
runReaderT m = runConveyorT m . repeat

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT m = do
    (a, xs) <- runGeneratorT m
    return (a, mconcat xs)

ex = do
    v <- flip runReaderT 42 $ do
        a <- ask
        return $ a * 2
    print v
    
    v <- runWriterT $ do
        tell [1,2,3]
        tell [4,5,6]
        tell [7,8,9]
    print v

main = ex
