import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import Prelude hiding (lookup)

-- <<types
type Name        = String
type PhoneNumber = String
type PhoneBook   = Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)
-- >>

-- <<new
new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return (PhoneBookState m)
-- >>

-- <<insert
insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  -- 式を未評価で格納しているので大量の insert があるとスペースリークする
  -- putMVar m (Map.insert name number book)
  -- $! は $ の評価版 ここで評価すると ロック時間が長くなる
  -- putMVar m $! Map.insert name number book
  -- 両方手に入れる方法
  let book' = Map.insert name number book
  putMVar m book'
  seq book' (return ())
-- >>

-- <<lookup
lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (Map.lookup name book)
-- >>

-- <<main
main = do
  s <- new
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  lookup s "name999" >>= print
  lookup s "unknown" >>= print
-- >>
