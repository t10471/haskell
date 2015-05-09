{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Data.Unique
import Control.Exception
import Data.Typeable

data Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show (Timeout _) = "timeout"

instance Exception Timeout

-- <<timeout-sig
timeout :: Int -> IO a -> IO (Maybe a)
-- >>

-- <<timeout
timeout t m
    -- 0以下ならタイムアウトしない
    | t <  0    = fmap Just m                           -- <1>
    -- 0ならタイムアウト
    | t == 0    = return Nothing                        -- <1>
    | otherwise = do
        -- 自分のthread ID を取得
        pid <- myThreadId                               -- <2>
        u <- newUnique                                  -- <3>
        let ex = Timeout u                              -- <3>
        handleJust                                      -- <4>
           -- 補足する例外を指定
           (\e -> if e == ex then Just () else Nothing) -- <5>
           -- 例外発生時の処理
           (\_ -> return Nothing)                       -- <6>
           -- handlejust で行うメイン処理 時間が過ぎたら親に例外を投げる
           (bracket (forkIO $ do threadDelay t          -- <7>
                                 throwTo pid ex)
                    -- 必ず行う処理 自分を殺す tid は forkIO の戻り値
                    (\tid -> throwTo tid ThreadKilled)  -- <8>
                    -- bracket で行うメイン処理
                    (\_ -> fmap Just m))                -- <9>
-- >>

main = (timeout 200000 $ timeout 100000 $ timeout 300000 $ threadDelay 1000000) >>= print
