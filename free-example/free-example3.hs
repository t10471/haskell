
import Control.Monad.Free
import Control.Monad.Trans.Free (FreeT) 
import Data.Char

data CharIO a = GetCh (Char -> a) | PutCh Char a

instance Functor CharIO where
    fmap f (GetCh g)   = GetCh (f . g)
    fmap f (PutCh c x) = PutCh c (f x)

-- 再帰している(Pureが内部にある)のでliftFは使えない
getCh :: Free CharIO Char
getCh = Free $ GetCh $ \ch -> Pure ch

putCh :: Char -> Free CharIO ()
putCh ch = Free $ PutCh ch (Pure ())
-- Pureで再帰を終了
runStdIO :: Free CharIO a -> IO a
runStdIO (Pure a)               = return a
runStdIO (Free (GetCh f))       = getChar >>= \ch -> runStdIO (f ch)
runStdIO (Free (PutCh ch cont)) = putChar ch >> runStdIO cont

ex :: Free CharIO ()
ex = do
    mapM_ putCh "Hello, Haskeller! Please input a character:"
    ch <- getCh
    mapM_ putCh "The ordinal of the character is:"
    mapM_ putCh (show (ord ch))
    mapM_ putCh ".\n Thank you!\n"

exec :: IO ()
exec = do
  runStdIO ex

-- tick を追加
-- tick は計算を中断

data SyncCharIO a = GetChS (Char -> a) | PutChS Char a | TickS a

instance Functor SyncCharIO where
    fmap f (GetChS g)   = GetChS (f . g)
    fmap f (PutChS c x) = PutChS c (f x)
    fmap f (TickS x)    = TickS (f x)

getChS :: Free SyncCharIO Char
getChS = Free $ GetChS $ \ch -> Pure ch

putChS :: Char -> Free SyncCharIO ()
putChS ch = Free $ PutChS ch (Pure ())

tickS :: Free SyncCharIO ()
tickS = Free $ TickS (Pure ()) 

-- 戻り値の型にFreeを含めることで計算をそのまま使える
-- 戻り値の型を変えたことによりPureも変更
runSyncStdIO :: Free SyncCharIO a -> IO (Free SyncCharIO a)
runSyncStdIO (Pure a)                = return (Pure a)
runSyncStdIO (Free (GetChS f))       = getChar >>= \ch -> runSyncStdIO (f ch)
runSyncStdIO (Free (PutChS ch cont)) = putChar ch >> runSyncStdIO cont
runSyncStdIO (Free (TickS cont))     = return cont

exS :: Free SyncCharIO ()
exS = do
    mapM_ putChS "Hello, Haskeller! Please input a character:"
    ch <- getChS
    mapM_ putChS "The ordinal of the character is:\n"
    tickS -- ここで一時中断
    mapM_ putChS (show (ord ch))
    mapM_ putChS ".\n Thank you!\n"

execS :: IO (Free SyncCharIO ())
execS = do
  -- tickSで中断
  x <- runSyncStdIO exS
  putStrLn "tick"
  -- 再開
  runSyncStdIO x

-- embedIOI を追加

data SyncCharIOEx a = GetChI (Char -> a) | PutChI Char a | TickI a | EmbedIO (IO a)

instance Functor SyncCharIOEx where
    fmap f (GetChI g)   = GetChI (f . g)
    fmap f (PutChI c x) = PutChI c (f x)
    fmap f (TickI x)    = TickI (f x)
    fmap f (EmbedIO m)  = EmbedIO (fmap f m)

getChI :: Free SyncCharIOEx Char
getChI = Free $ GetChI $ \ch -> Pure ch

putChI :: Char -> Free SyncCharIOEx ()
putChI ch = Free $ PutChI ch (Pure ())

tickI :: Free SyncCharIOEx ()
tickI = Free $ TickI (Pure ())

embedIOI :: IO a -> Free SyncCharIOEx a
embedIOI m = Free $ EmbedIO $ fmap return m

runSyncStdIOEx :: Free SyncCharIOEx a -> IO (Free SyncCharIOEx a)
runSyncStdIOEx (Pure a)                = return (Pure a)
runSyncStdIOEx (Free (GetChI f))       = getChar >>= \ch -> runSyncStdIOEx (f ch)
runSyncStdIOEx (Free (PutChI ch cont)) = putChar ch >> runSyncStdIOEx cont
runSyncStdIOEx (Free (TickI cont))     = return cont
runSyncStdIOEx (Free (EmbedIO m))      = m >>= runSyncStdIOEx

exI :: Free SyncCharIOEx ()
exI = do
    embedIOI $ putStrLn "execute embedIOI"
    mapM_ putChI "Hello, Haskeller! Please input a character:"
    ch <- getChI
    mapM_ putChI "The ordinal of the character is:\n"
    embedIOI $ putStrLn "emb" -- IOを実施
    mapM_ putChI (show (ord ch))
    mapM_ putChI ".\n Thank you!\n"

execI :: IO (Free SyncCharIOEx ())
execI = do
  runSyncStdIOEx exI

main :: IO ()
main = do
  putStrLn "end"
