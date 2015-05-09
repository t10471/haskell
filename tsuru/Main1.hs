
import Control.Monad.Identity
import Control.Monad.Trans.Except 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.IO
import System.Environment
import System.Directory
import Debug.Trace
import Network.Pcap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Numeric (showHex)
import Data.Bits
import Data.List
import Data.Word
import Data.UnixTime
import Text.Printf

data Env = Env {
              file      :: FilePath
            , isReOrder :: Bool
            } deriving Show
type Args = [String]

type EnvT a = ReaderT  Args (ExceptT String
                           (WriterT [String] (StateT Env IO))) a

runEnvT :: Args -> Env -> EnvT a -> IO ((Either String a, [String]), Env)
runEnvT args env ev =
      runStateT (runWriterT (runExceptT (runReaderT ev args))) env

initEnv = Env {file = "", isReOrder = False}
maxLenght = 2
rFlg = "-r"
remove t []     = []
remove t (x:xs) = rm t x xs
  where rm t x xs
          | t == x    = remove t xs
          | otherwise = x : remove t xs 

onReOrder :: EnvT ()
onReOrder = do
  x <- get
  put x {isReOrder = True}

setFile :: String -> EnvT ()
setFile f = do
  x <- get
  put x {file = f}

evalArgs :: EnvT ()
evalArgs = do
  xs <- ask
  when (null xs) $ lift $ throwE "empty args"
  when (length xs > maxLenght) $ lift $ throwE "too many args"
  loopEvalArgs
  return ()

loopEvalArgs :: EnvT ()
loopEvalArgs = do
  xs <- ask
  if elem rFlg xs then
    onReOrder >> local (remove rFlg) loopEvalArgs
  else
    if length xs /= 1 then
      tell ["invalid args"]
    else do
      let f = head xs 
      b <- liftIO $ doesFileExist f
      if b then
        setFile f
      else
        tell ["file not exists"]
  return ()

parseArgs :: Args -> IO ([String], Env)
parseArgs args = do
  ((ret, msgs), env) <-  runEnvT args initEnv evalArgs 
  case ret of
    Left msg -> return ([msg], env)
    Right d  -> return (msgs, env)

prettyPrint :: BS.ByteString -> String
prettyPrint = concat . map (flip showHex "") . BS.unpack

slice :: Int -> Int -> BS.ByteString -> BS.ByteString
slice s e b = BS.take e $ BS.drop s b

toInt :: BS.ByteString -> Int
toInt = toInt' . BS.unpack

toInt' :: [Word8] -> Int
toInt' []       = 0
toInt' [x]      = fromIntegral x
toInt' z@(x:xs) = (shiftL (fromIntegral x) ((length z - 1) * 8)) + toInt' xs

bi xs = foldl (\x y -> 2 * x + y) 0 $ map (\x -> read [x]) xs

timeFormat :: Format
timeFormat = C8.pack "%H%M%S"

getUpd :: C8.ByteString -> C8.ByteString
getUpd q =
    let etheHL = 14
        ipi    = head $ BS.unpack $ slice etheHL 1 q
        ipHL   = fromIntegral (ipi .&. (bi "00001111")) * 4
        updHP  = etheHL + ipHL
    in C8.drop (updHP + 8) q

getDIM :: C8.ByteString -> String
getDIM upd = C8.unpack $ slice 0 5 upd

getPacketTime :: PktHdr -> IO String
getPacketTime p = do
    let u = printf "%02d" (round $ fromIntegral (hdrUseconds p) / 10000 :: Int) :: String
        t = UnixTime  { utSeconds      = fromIntegral (hdrSeconds p) 
                      , utMicroSeconds = fromIntegral (hdrUseconds p)}
    p <- formatUnixTime timeFormat t
    return $ (C8.unpack p) ++ u

data Trade = Td { price :: Int, quantity :: Int} deriving Show
data Market = Mkt {
                pktTime    :: String
              , acceptTime :: String
              , issueCcode :: String
              , mBids      :: [Trade]
              , mAsks      :: [Trade]
              } deriving Show

getInt :: Int -> Int -> C8.ByteString -> Int
getInt s b upd = read $ getString s b upd

getString :: Int -> Int -> C8.ByteString -> String 
getString s b upd = C8.unpack $ slice s b upd

getTrade s upd = Td {
                    price    = getInt s     5 upd
                  , quantity = getInt (s+5) 7 upd 
                 }

getBids :: Int -> C8.ByteString -> [Trade]
getBids s upd = reverse $ fst $ foldr f ([], s) (replicate 5 12)
  where f x (ys,p) = ((getTrade p upd):ys, p + x)

getMarket :: PktHdr -> C8.ByteString -> IO Market
getMarket h upd = do
    packerTime <- getPacketTime h
    return $ Mkt {
        pktTime    = packerTime 
      , acceptTime = getString 206 8 upd
      , issueCcode = getString 5  12 upd
      , mBids      = getBids 29 upd 
      , mAsks      = getBids 96 upd
    }

type MarketT a = StateT [Market] IO a 
runMarketT :: [Market] -> MarketT a -> IO (a, [Market])
runMarketT st m = runStateT m st

callfun :: PcapHandle -> Int -> MarketT ()
callfun f i = do
    (p,q) <- liftIO $ next f >>= toBS 
    if hdrWireLength p /= 0 then do
      let upd = getUpd q
      if getDIM upd /= "B6034" then
        callfun f $ i + 1
      else do
        m <- liftIO $ getMarket p upd
        xs <- get
        put $ m:xs
        -- when (i < 100) $ callfun f $ i + 1
        callfun f $ i + 1
        return ()
    else
      return ()

parsePcap :: Env -> IO [Market]
parsePcap env = do
  hd <- openOffline (file env)
  setFilter hd "udp and (dst port 15515 or dst port 15516)" False 0
  (d, m) <- runMarketT [] $ callfun hd 0 
  return m

printTrades ::[Trade] -> IO ()
printTrades ts = mapM_ pr ts
  where 
    pr t = putStr $  " " ++ q ++ "@" ++ p
      where 
        q = show (quantity t)
        p = show (price t)


printMarket :: Market -> IO ()
printMarket m = do
  putStr $ pktTime m ++ " "
  putStr $ acceptTime m ++ " "
  putStr $ issueCcode m
  printTrades $ reverse $ mBids m 
  printTrades $ mAsks m 
  putStr "\n"

sortMarkets ms env = if isReOrder env then
                sortBy (\x y -> compare (acceptTime x) (acceptTime y)) ms
              else
                reverse ms

printMarkets :: [Market] -> Env -> IO ()
printMarkets ms env = do
    mapM_ printMarket $ sortMarkets ms env
    return ()

main :: IO ()
main = do
  args <- getArgs
  (msgs, env) <- parseArgs args
  when (not (null msgs)) $ do 
    mapM_ putStrLn msgs
    return ()
  m <- parsePcap env
  printMarkets m env
  putStrLn "end"
