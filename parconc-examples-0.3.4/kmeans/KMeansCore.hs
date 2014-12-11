--
-- Adapted from the K-Means example in the remote-0.1.1 package,
--   (c) Jeff Epstein <jepst79@gmail.com>
--

{-# LANGUAGE DeriveDataTypeable #-}
module KMeansCore where

import Data.List
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import Data.Binary
import Control.DeepSeq

-- -----------------------------------------------------------------------------
-- Points
-- UNPACKはポインタを実の値にする?
-- !をつけると正格になる
-- bang patternという
data Point = Point {-#UNPACK#-}!Double {-#UNPACK#-}!Double
    deriving (Show,Read,Eq)

instance NFData Point

-- <<point-ops
zeroPoint :: Point
zeroPoint = Point 0 0

-- 距離の比較に使うからルートの計算はしない
sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1-x2)^2) + ((y1-y2)^2)
-- >>

instance Binary Point where
  put (Point a b) = put a >> put b
  get = do a <- get; b <- get; return (Point a b)

readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points = [ Point (read (B.unpack sx)) (read (B.unpack sy))
               | (sx:sy:_) <- ls ]
  --
  return points

-----------------------------------------------------------------------------
-- Clusters
-- clId クラスタの番号
-- clCent 重心
data Cluster
  = Cluster { clId    :: {-# UNPACK #-} !Int
            , clCent  :: {-# UNPACK #-} !Point
            }
  deriving (Show,Read,Eq)

instance NFData Cluster  -- default is ok, all the fields are strict


makeCluster :: Int -> [Point] -> Cluster
makeCluster clid points =
  Cluster { clId    = clid
          , clCent  = Point (a / fromIntegral count) (b / fromIntegral count)
          }
 where
  pointsum@(Point a b) = foldl' addPoint zeroPoint points
  count = length points

  addPoint :: Point -> Point -> Point
  addPoint (Point a b) (Point c d) = Point (a+c) (b+d)
