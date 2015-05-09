import Control.Concurrent
import GetURL
import qualified Data.ByteString as B
import Text.Printf
import Control.Monad

-- <<main
sites = ["http://www.google.com",
         "http://www.bing.com",
         "http://www.yahoo.com",
         "http://www.wikipedia.com/wiki/Spade",
         "http://www.wikipedia.com/wiki/Shovel"]

main :: IO ()
main = do
  m <- newEmptyMVar
  let
    download url = do
       r <- getURL url
       putMVar m (url, r)

  mapM_ (forkIO . download) sites

  (url, r) <- takeMVar m
  -- 最初に帰ってきたものだけ表示V
  printf "%s was first (%d bytes)\n" url (B.length r)
  -- 残りは捨てる
  replicateM_ (length sites - 1) (takeMVar m)
-- >>

