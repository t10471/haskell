{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Market (
      Trade(..)
    , Quote(..)
    , printQuotes
    , parsePcap
) where 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Numeric (showHex)
import Data.Bits
import Data.List
import Data.Word
import Data.UnixTime
import Text.Printf
import Network.Pcap
import Control.Monad.State
import Control.Applicative
import Arg

data Trade = Td { price :: Int, quantity :: Int} deriving Show
data Quote = Qt { pktTime    :: String
                , acceptTime :: String
                , issueCcode :: String
                , mBids      :: [Trade]
                , mAsks      :: [Trade]
                } deriving Show

newtype QuoteT a = QuoteT {runQuoteT :: StateT [Quote] IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState [Quote])
runQuote :: [Quote] -> QuoteT a -> IO (a, [Quote])
runQuote st m = runStateT (runQuoteT m) st

pprint :: BS.ByteString -> String
pprint = concat . map (flip showHex "") . BS.unpack

slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice s e b = BS.take e $ BS.drop s b

bs2i :: BS.ByteString -> Int
bs2i = w82i . BS.unpack

w82i :: [Word8] -> Int
w82i []       = 0
w82i [x]      = fromIntegral x
w82i z@(x:xs) = (shiftL (fromIntegral x) ((length z - 1) * 8)) + w82i xs

s2b :: String -> Word8
s2b xs = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) xs

timeFormat :: Format
timeFormat = C8.pack "%H%M%S"

getUpd :: C8.ByteString -> C8.ByteString
getUpd q =  let etheHL = 14
                ipi    = head $ BS.unpack $ slice etheHL 1 q
                ipHL   = fromIntegral (ipi .&. (s2b "00001111")) * 4
    in C8.drop (etheHL + ipHL + 8) q

getPacketTime :: PktHdr -> IO String
getPacketTime p = do
    let u = printf "%02d" (round $ fromIntegral (hdrUseconds p) / 10000 :: Int) :: String
        t = UnixTime  { utSeconds      = fromIntegral (hdrSeconds p) 
                      , utMicroSeconds = fromIntegral (hdrUseconds p)}
    p <- formatUnixTime timeFormat t
    return $ (C8.unpack p) ++ u

getInt :: Int -> Int -> C8.ByteString -> Int
getInt s b upd = read $ getString s b upd

getString :: Int -> Int -> C8.ByteString -> String 
getString s b upd = C8.unpack $ slice s b upd

getTrade s upd = Td { price    = getInt s     5 upd
                    , quantity = getInt (s+5) 7 upd 
                    }

getTrades :: Int -> C8.ByteString -> [Trade]
getTrades s upd = reverse $ fst $ foldr f ([], s) $ replicate 5 12
  where f x (ys,p) = ((getTrade p upd):ys, p + x)

getQuote :: PktHdr -> C8.ByteString -> IO Quote
getQuote h upd = do
    packerTime <- getPacketTime h
    return $ Qt {
        pktTime    = packerTime 
      , acceptTime = getString 206 8 upd
      , issueCcode = getString 5  12 upd
      , mBids      = getTrades 29 upd 
      , mAsks      = getTrades 96 upd
    }

evalQuote :: PcapHandle -> Int -> QuoteT ()
evalQuote f i = do
    (p,q) <- liftIO $ next f >>= toBS 
    call p q
  where 
    call p q = do
      if | hdrWireLength p == 0   -> return ()
         | getHead upd /= "B6034" -> evalQuote f $ i + 1
         | otherwise              -> do
            m <- liftIO $ getQuote p upd
            xs <- get
            put $ m:xs
            -- when (i < 100) $ evalQuote f $ i + 1
            evalQuote f $ i + 1
      where upd = getUpd q
    getHead upd = getString 0 5 upd

parsePcap :: Env -> IO [Quote]
parsePcap env = do
  hd <- openOffline (file env)
  setFilter hd "udp and (dst port 15515 or dst port 15516)" False 0
  (d, m) <- runQuote [] $ evalQuote hd 0 
  return m

printTrades ::[Trade] -> IO ()
printTrades ts = mapM_ p ts
  where p t = let q = show (quantity t)
                  p = show (price t)
          in putStr $  " " ++ q ++ "@" ++ p

printQuote :: Quote -> IO ()
printQuote m = do
  putStr $ pktTime m ++ " "
  putStr $ acceptTime m ++ " "
  putStr $ issueCcode m
  printTrades $ reverse $ mBids m 
  printTrades $ mAsks m 
  putStr "\n"

printQuotes :: [Quote] -> Env -> IO ()
printQuotes ms env = mapM_ printQuote $ s ms env
  where
    s ms env = if isReOrder env then sortBy c ms else reverse ms
    c x y    = compare (acceptTime x) (acceptTime y)
