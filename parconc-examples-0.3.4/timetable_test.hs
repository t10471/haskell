
import Debug.Trace
import Data.List
import Control.DeepSeq

import Data.Map (Map)
import qualified Data.Map as Map

-- <<Talk
newtype Talk = Talk Int
  deriving (Eq,Ord)

instance NFData Talk

instance Show Talk where
  show (Talk t) = show t
-- >>

-- <<Person
data Person = Person
  { name  :: String
  -- 見たい発表
  , talks :: [Talk]
  }
  deriving (Show)
-- >>

clashes :: Map Talk [Talk]
clashes = Map.fromListWith union
   [ (t, ts)
   | s <- testPersons 
   , (t, ts) <- selects (talks s) ]

-- <<selects
selects :: [a] -> [(a,[a])]
selects xs0 = go [] xs0
  where
   go xs [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys
-- >>

cs@[c1,c2,c3,c4] = map Talk [1..4]

testPersons =
 [ Person "P" [c1,c2]
 , Person "Q" [c2,c3]
 , Person "R" [c3,c4]
 , Person "S" [c1,c4]
 ]

main = do
  print [ (t, ts) | s <- testPersons , (t, ts) <- selects (talks s) ]
  print clashes
  mapM_ (\x -> print $ Map.findWithDefault [] x clashes) cs 
