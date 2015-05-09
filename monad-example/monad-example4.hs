
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Cont
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

data Entry = Log  { timestamp :: UTCTime
                  , msg       :: String
                  } deriving Eq

instance Show Entry where
  show (Log t s) = (show t) ++ " | " ++ s

type LogWriter = WriterT [Entry] IO 

logMsg :: String -> LogWriter ()
logMsg s = do 
    t <- liftIO getCurrentTime
    tell [Log t s]

-- ContT と WriterT の合成
contWT :: Int -> ContT String LogWriter String
contWT n = do
    str <- callCC $ \exit1 -> do
      n' <- callCC $ \exit2 -> do
        when (n < 3) $ do
          lift $ logMsg $ "n < 3 " ++ show n
          x <- contWT $ n + 1
          exit1 x
        when (n < 7) $ do 
          lift $ logMsg $ "n < 7 " ++ show n
          exit2 n
        lift $ logMsg "other"
        return n
      when (n < 10) $ do
          x <- contWT $ n + 1
          exit1 x
      return $ "return value = " ++ show n'
    return str

runContWT :: Int -> IO (String, [Entry])
runContWT i = do
    (s, e) <- runWriterT (runContT (contWT i) return)
    return (s, e)

exContWT :: IO ()
exContWT = do
    (s, e) <- runContWT 0
    mapM print e
    print s
    return ()

-- ReaderT と ContT の合成
-- lift しなくていいのが謎
contRT :: ReaderT Int (ContT String IO) String
contRT = do
    n <- ask
    str <- callCC $ \exit1 -> do
      n' <- callCC $ \exit2 -> do
        when (n < 3) $ do
          x <- local (+1) contRT
          exit1 x
        when (n < 7) $ do 
          exit2 n
        return n
      when (n < 10) $ do
          x <- local (+1) contRT
          exit1 x
      return $ "return value = " ++ show n'
    return str

runContRT :: Int -> IO (String)
runContRT i = do
  n <- runContT (runReaderT contRT i) return
  return n

exContRT :: IO ()
exContRT = do
    n <- runContRT 0
    print n
    return ()

-- 逆version
contRT' :: ContT String (ReaderT Int IO) String
contRT' = do
    n <- lift $ ask
    str <- callCC $ \exit1 -> do
      n' <- callCC $ \exit2 -> do
        when (n < 3) $ do
          x <- local (+1) contRT'
          exit1 x
        when (n < 7) $ do 
          exit2 n
        return n
      when (n < 10) $ do
          x <- local (+1) contRT'
          exit1 x
      return $ "return value = " ++ show n'
    return str

runContRT' :: Int -> IO (String)
runContRT' i = do
  n <- runReaderT (runContT contRT' return) i
  return n

exContRT' :: IO ()
exContRT' = do
    n <- runContRT' 0
    print n
    return ()


main :: IO ()
main = do
    exContWT 
    exContRT 
    exContRT' 
    putStrLn "end"
