{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Tue Aug 19 09:31:32 2003>
   License:    GPL
-}
{- DESCRIPTION
 runhaskell ex-writer.hs rules.txt packets.txt
-}

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Writer
import System.Environment

data Data   = AnyData | Data String deriving (Show,Read)
data Addr   = AnyHost | Host Int    deriving (Show,Read)
data Packet = Packet  { from    :: Addr
                      , to      :: Addr
                      , payload :: Data
                      } deriving (Eq,Show,Read)
instance Eq Data where
  AnyData   == _         = True
  _         == AnyData   = True
  (Data s1) == (Data s2) = s1 == s2
  
instance Eq Addr where
  AnyHost   == _         = True
  _         == AnyHost   = True
  (Host h1) == (Host h2) = h1 == h2

data Disposition = Accept | Reject deriving (Eq,Show,Read)
data Rule = Rule  { disposition :: Disposition
                  , pattern     :: Packet
                  , logIt       :: Bool
                  } deriving (Eq,Show,Read)

matchPacket :: Packet -> Rule -> Maybe Rule
matchPacket packet rule = if pattern rule == packet then Just rule else Nothing

match :: [Rule] -> Packet -> Maybe Rule
match rules packet = foldl mplus Nothing (map (matchPacket packet) rules)

data Entry = Log { count :: Int
                 , msg   :: String
                 } deriving Eq

instance Show Entry where
  show (Log 1 s) = s
  show (Log n s) = (show n) ++ " X " ++ s

logMsg :: String -> Writer [Entry] ()
logMsg s = tell [Log 1 s]

mergeEntries :: [Entry] -> [Entry] -> Writer [Entry] [Entry]
mergeEntries []   x    = return x
mergeEntries x    []   = return x
mergeEntries [e1] [e2] = let (Log n  msg)  = e1
                             (Log n' msg') = e2
                         in if msg == msg' then
                              return [(Log (n+n') msg)]
                            else
                              do 
                                tell   [e1]
                                return [e2]

filterOne :: [Rule] -> Packet -> Writer [Entry] (Maybe Packet)
filterOne rules packet = do 
    rule <- return (match rules packet)
    case rule of
      Nothing  -> do 
        logMsg ("DROPPING UNMATCHED PACKET: " ++ (show packet))
        return Nothing
      (Just r) -> do 
        when (logIt r) (logMsg ("MATCH: " ++ (show r) ++ " <=> " ++ (show packet)))
        case r of
          (Rule Accept _ _) -> return (Just packet)
          (Rule Reject _ _) -> return Nothing

groupSame :: (Monoid a) => a -> (a -> a -> Writer a a) -> [b] -> (b -> Writer a c) -> Writer a [c]
groupSame initial merge []     _  = do 
    tell initial
    return []
groupSame initial merge (x:xs) fn = do 
    (result,output) <- return (runWriter (fn x))
    new             <- merge initial output
    rest            <- groupSame new merge xs fn
    return (result:rest)

filterAll :: [Rule] -> [Packet] -> Writer [Entry] [Packet]
filterAll rules packets = do 
    tell [Log 1 "STARTING PACKET FILTER"]
    out <- groupSame [] mergeEntries packets (filterOne rules)
    tell [Log 1 "STOPPING PACKET FILTER"]
    return (catMaybes out)

main :: IO ()
main = do 
    args       <- getArgs
    ruleData   <- readFile (args!!0)
    packetData <- readFile (args!!1)
    let rules     = (read ruleData)   :: [Rule]
        packets   = (read packetData) :: [Packet]
        (out,log) = runWriter (filterAll rules packets)
    putStrLn "ACCEPTED PACKETS"
    putStr (unlines (map show out))
    putStrLn "\n\nFIREWALL LOG"
    putStr (unlines (map show log))

