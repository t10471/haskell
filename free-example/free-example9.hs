{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor    #-}

import System.Random
import Control.Monad
import Control.Monad.Free
import Control.Monad.Random
import Control.Applicative
import Control.Arrow (first,(&&&),(***),Arrow,arr)
import Data.List (sortBy,maximumBy)
import Data.Bits
import Data.Ord (comparing)
import Data.Word
import qualified Data.Vector as V

{-
Cross    遺伝子を2つ取って交叉します。交叉した遺伝子を得ます。
Mutate   遺伝子を突然変異させます。突然変異した遺伝子を得ます。
Archive  遺伝子を次の世代に残します。現時点までにarchiveした数 compare 母集団サイズを得ます。
AtRandom 重みに基づいて次の操作を選択し、選ばれた操作から遺伝子を得ます。
GoNext   計算を次の世代に進めます。世代数と新たな遺伝子集団とその評価値を得ます。
Extinct  強制終了します。
-}
data GA gene e a =
    Cross    gene gene (gene -> a)
  | Mutate   gene      (gene -> a)
  | Archive  gene  (Ordering -> a)
  | AtRandom [(Free (GA gene e) gene, Rational)] (gene -> a)
  | GoNext   ((Int, [(gene,e)]) -> a)
  | Extinct
  deriving (Functor)
type Nature gene e = Free (GA gene e)

cross      x y = liftF $ Cross   x y id
mutate     x   = liftF $ Mutate  x   id
archive    x   = liftF $ Archive x   id
archiveAll xs  = mapM_ archive xs
atRandom   xs  = liftF $ AtRandom xs id
goNext :: MonadFree (GA gene e) m => m (Int, [(gene,e)])
goNext         = liftF $ GoNext id
extinct :: MonadFree (GA gene e) m => m a
extinct        = liftF Extinct

-- -- *** は 2つの関数を1つにする
-- arrCn :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a (Int, Int) (Int, Int)
-- arrCn f g = arr f *** arr g
-- -- &&& は 2つの関数の結果を1つにする
-- arrCr :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a Int (Int, Int)
-- arrCr f g = arr f &&& arr g
-- exA = do
--     let up   = (+1) :: Int -> Int
--         down = flip (-) 1 :: Int -> Int
--     print $ arrCn up down $ (1,2)
--     print $ arrCr up down $ 1

rouletteSelect :: (Real e, MonadFree (GA a e) m) => [(a,e)] -> m a
rouletteSelect xs = atRandom $ map (return *** toRational) xs
randomSelect xs = atRandom $ map (return &&& (const 1.0)) xs

data Env gene =  Env {
                    popLimit :: Int
                  , genCount :: Int
                  , children :: V.Vector gene
                  }

runNature
  :: (RandomGen g, Random gene, Real e) =>
      Env gene
      -> (gene -> gene -> Rand g gene)
      -> (gene -> Rand g gene)
      -> (gene -> e)
      -> Nature gene e b
      -> Rand g (Maybe b)
runNature env cr mu ef = run env where
  run env@(Env lim cnt cs) nature = case nature of
    Free (Cross x y cont)    -> cr x y >>= run env . cont
    Free (Mutate x cont)     -> mu x   >>= run env . cont
    Free (Archive x cont)
      | lim == csize + 1     -> run env{children=V.cons x cs} $ cont EQ
      | lim > csize          -> run env{children=V.cons x cs} $ cont LT
      | otherwise            -> run env $ cont GT
      where csize = V.length cs
    Free (AtRandom xs cont)  -> fromList xs >>= \act -> run env (act >>= cont)
    Free (GoNext cont)
      | cnt <= 0             -> do 
                                roots <- replicateM lim (liftRand random)
                                run upenv $ cont (0    , map (id &&& ef) roots)
      | otherwise            -> run upenv $ cont (cnt+1, map (id &&& ef) (V.toList cs))
        where upenv = env{genCount=cnt+1, children=V.empty}
    Free Extinct             -> return Nothing
    Pure x                   -> return $ Just x

{-
上位10体を次世代に持ち越し
母集団サイズに達するまでルーレット選択で2体選んで交叉かルーレット選択で1体選んで突然変異
97%で前者、3%で後者
終了条件:200世代で終了
-}
ex :: (MonadFree (GA gene e) m, Real e) => m e
ex = do
  (i,gs) <- goNext
  when (null gs) extinct
  let top10 = take 10 $ sortBy (flip (comparing snd)) gs
  if i > 200
    then return $ snd $ head top10
    else do
      archiveAll $ map fst top10
      selectRest gs >> ex
  where
    selectRest xs = do
      let rs = rouletteSelect xs
      g   <- atRandom $ (first join) <$> [(cross <$> rs <*> rs, 0.97),(mutate <$> rs, 0.03)]
      pop <- archive g
      when (pop < EQ) $ selectRest xs

-- 解く問題
problem gene = 
  let x = (fromIntegral gene) / (fromIntegral (maxBound :: Word32))
  in  sin(3 * x) + 0.5 * sin(9 * x) + sin(15 * x + 50)

initEnv = Env {
  popLimit=50,
  genCount=0,
  children= V.empty :: V.Vector Word32}

{-
母集団サイズ:50
遺伝子の型: 32bit符号無し整数
交叉方法:一点交叉
突然変異:ビット反転
評価関数:遺伝子を実数にしてf(x)に適用した値
-}
main :: IO ()
main = do
  g   <- getStdGen
  ans <- return $ evalRand (runNature initEnv cf mf problem ex) g
  print $ ans
  where 
    -- 突然変異する関数
    mf gene = return $ complement gene
    -- 交差する関数
    cf g1 g2 = do
      pos <- liftRand (randomR (0,31)) 
      let mask = (shiftL 1 pos) - 1
      return $ (g1 .&. (complement mask)) .|. (mask .&. g2)

