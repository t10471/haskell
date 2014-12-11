import KMeansCore
import Data.Random.Normal
import System.Random
import System.IO
import Data.Array
import System.Environment
import Control.Monad
import Data.List
import Data.Binary

minX, maxX, minY, maxY, minSD, maxSD :: Double
minX = -10
maxX = 10
minY = -10
maxY = 10
minSD = 1.5
maxSD = 2.0

main = do
  -- fmap :: Functor f => (a -> b ) -> f a -> f b
  -- fmap read :: (Read b, Functor f) => f String -> f b
  -- getArgs :: IO [String]
  -- fmap (fmap read) getArgs :: Read b => IO [b]
    n: minp: maxp: rest <- fmap (fmap read) getArgs

    case rest of
        -- 最後の引数は配列になっている
        -- 乱数の初期化
        [seed] -> setStdGen (mkStdGen seed)
        _ -> return ()

    nps <- replicateM n (randomRIO (minp, maxp))
    xs  <- replicateM n (randomRIO (minX, maxY))
    ys  <- replicateM n (randomRIO (minX, maxY))
    sds <- replicateM n (randomRIO (minSD, maxSD))

    let params = zip5 nps xs ys sds sds

    -- first generate a set of points for each set of sample parameters
    -- ( (Int, Double, Double, Double, Double) 
    --    -> IO [Point]) 
    -- -> [(Int, Double, Double, Double, Double)] 
    -- -> IO [[Point]]
    ss <- mapM (\(a,b,c,d,e) -> generate2DSamples a b c d e) params
    let points = concat ss

    -- dump all the points into the file "points"
    hsamp <- openFile "points" WriteMode
    -- mapM_は結果を捨てる
    -- (Point -> IO ()) -> [Point] -> IO ()
    mapM_ (printPoint hsamp) points
    hClose hsamp

    encodeFile "points.bin" points

    -- generate the initial clusters by assigning each point to random
    -- cluster.
    -- 新しい乱数機を生成
    gen <- newStdGen
    let
        rand_clusters = randomRs (0,n-1) gen :: [Int]
        arr = accumArray (flip (:)) [] (0,n-1) $
                zip rand_clusters points
        clusters = map (uncurry makeCluster) (assocs arr)
    writeFile "clusters" (show clusters)

    -- so we can tell what the answer should be:
    writeFile "params" (show params)


printPoint :: Handle -> Point -> IO ()
printPoint h (Point x y) = do
  hPutStr h (show x)
  hPutChar h ' '
  hPutStr h (show y)
  hPutChar h '\n'

generate2DSamples :: Int                 -- number of samples to generate
                  -> Double -> Double    -- X and Y of the mean
                  -> Double -> Double    -- X and Y standard deviations
                  -> IO [Point]

generate2DSamples n mx my sdx sdy = do
  gen <- newStdGen
  let (genx, geny) = split gen
      -- 正規分布に基づくサンプリングをおこなう平均と分散、Randomのシードを渡す
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return (zipWith Point (take n xsamples) ysamples)

