import Network
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import ConcurrentUtils (forkFinally)
import Debug.Trace

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  -- atomically :: STM (TVar Integer) -> IO (TVar Integer)
  -- <- で IO をとる
  factor <- atomically $ newTVar 2                               -- <1>
  forever $ do
    (handle, host, port) <- accept sock
    printf "Accepted connection from %s: %s\n" host (show port)
    forkFinally (talk handle factor) (\_ -> hClose handle)       -- <2>
    putStrLn "forkFinally end"
    

port :: Int
port = 44444
-- >>

-- <<talk
talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan              -- <1>
  -- race は一方が例外をなげるか、一方が完了すると
  -- 両方終了する
  -- ここでは receive は無限ループ
  -- server は end を入力されたときに終了する
  race (server h factor c) (receive h c)  -- <2>
  putStrLn "talk end"
  return ()
-- >>

-- <<receive
receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  putStrLn "receive hGetLin before"
  line <- hGetLine h
  putStrLn ("receive hGetLin " ++ line)
  atomically $ writeTChan c line
-- >>

-- <<server
server :: Handle -> TVar Integer -> TChan String -> IO ()
server h factor c = do
  f <- atomically $ readTVar factor     -- <1>
  hPrintf h "Current factor: %d\n" f    -- <2>
  loop f                                -- <3>
 where
  loop f = do
    -- readTVar readtchan で待機していたどちらかが変更されたら起きる
    action <- atomically $ do           -- <4>
      f' <- readTVar (trace ("serve readTVar before") factor)             -- <5>
      if (f /= (trace ("serve readTVar after " ++ show (f')) f'))                      -- <6>
         then return (newfactor f')     -- <7>
         else do
           l <- readTChan (trace ("serve readTChan before") c)             -- <8>
           return (command f (trace ("serve readTChan after " ++ l) l))         -- <9>
    action

  newfactor f = do                      -- <10>
    hPrintf h "new factor: %d\n" f
    putStrLn ("newfactor end ")
    loop f

  command f s                           -- <11>
   = case s of
      "end" ->
        hPutStrLn h ("Thank you for using the " ++
                     "Haskell doubling service.")         -- <12>
      '*':s -> do
        atomically $ writeTVar factor (read s :: Integer) -- <13>
        loop f
      line  -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f
-- >>

