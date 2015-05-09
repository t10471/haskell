
import Control.Category hiding (id, (.)) -- Prelude との衝突防止
import qualified Control.Category as Cat
import Control.Monad
import Control.Arrow

-- Category を実装した List 的なものを使って
-- Category の基礎のサンプル
-- 結局は Kleisli を使えば独自に作成する必要はないけどね・・・

newtype NonDet i o = NonDet { runNonDet :: i -> [o] }

instance Category NonDet where
    id = NonDet return
    (NonDet f) . (NonDet g) = NonDet $ f <=< g
    {- 以下と同じ
    id = NonDet (\x -> [x])
    (NonDet f) . (NonDet g) = NonDet $ concat . map f . g
    -}

succ_n :: Int -> Int -> [Int]
succ_n n x = [x..x+n]

succ_n_ND :: Int -> NonDet Int Int
succ_n_ND n = NonDet (succ_n n)

exA :: IO ()
exA = do
  print $ runNonDet (succ_n_ND 2) 10
  -- [10,11,12]
  -- 4 * 3 の配列
  print $ runNonDet ((succ_n_ND 2) Cat.. (succ_n_ND 3)) 10
  -- [10,11,12,
  --  11,12,13,
  --  12,13,14,
  --  13,14,15]
  print $ runNonDet ((succ_n_ND 2) Cat.. (succ_n_ND 2) Cat.. (succ_n_ND 2)) 10
  -- [10,11,12, 11,12,13, 12,13,14, 11,12,13, 12,13,14, 13,14,15, 12,13,14, 13,14,15, 14,15,16]
  -- Cat.id は id と一緒
  print $ runNonDet (Cat.id) 10
  print $ runNonDet (Cat.id Cat.. Cat.id) 10
  print $ runNonDet ((succ_n_ND 5) Cat.. Cat.id Cat.. (succ_n_ND 2) Cat.. Cat.id) 10
  -- >>> は Cat.. と一緒
  print $ runNonDet ((succ_n_ND 5) <<< (succ_n_ND 2)) 10
  print $ runNonDet ((succ_n_ND 2) >>> (succ_n_ND 5)) 10
  print $ runNonDet ((succ_n_ND 5) >>> (succ_n_ND 2)) 10

-- Maybe で Category

newtype Possible i o = Possible { runPossible :: i -> Maybe o } 
-- List と一緒
instance Category Possible where
    id = Possible return
    (Possible f) . (Possible g) = Possible $ f <=< g
    -- concat は List に特化した join
    -- id = Possible (\x -> Just x)
    -- (Possible f) . (Possible g) = Possible $ join . fmap f . g

biggerDouble :: (Ord t, Num t) => t -> t -> Maybe t
biggerDouble n x
  | n < x     = Just (x * 2)
  | otherwise = Nothing

biggerDoubleP :: (Ord t, Num t) => t -> Possible t t
biggerDoubleP n = Possible (biggerDouble n)

exB :: IO ()
exB = do
  print $ runPossible ((biggerDoubleP 10) Cat.. (biggerDoubleP 2)) 10
  print $ runPossible ((biggerDoubleP 10) <<< (biggerDoubleP 2)) 10
  print $ runPossible ((biggerDoubleP 10) Cat.. (biggerDoubleP 2)) 3
  print $ runPossible ((biggerDoubleP 10) Cat.. (biggerDoubleP 2) Cat.. (biggerDoubleP 2)) 3
  print $ runPossible (Cat.id <<< (biggerDoubleP 10) <<< Cat.id <<< (biggerDoubleP 2) <<< (biggerDoubleP 2) <<< Cat.id) 3
  print $ runPossible ((biggerDoubleP 10) <<< (biggerDoubleP 2) <<< (biggerDoubleP 4)) 3

-- 上で作成した Possible は Kleisli に置き換えられる

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

exC :: IO ()
exC = do
  print $ runPossible (Possible (landRight 2) >>> Possible (landLeft 2) >>> Possible (landRight 2)) (0, 0)
  print $ runPossible (Possible (landRight 1) >>> Possible (landLeft 4) >>> Possible (landRight (-1)) >>> Possible (landRight 2)) (0, 0)
  print $ runPossible (Possible (landLeft 1) >>> Possible banana >>> Possible (landRight 1)) (0, 0)
  print $ runKleisli (Kleisli (landRight 2) >>> Kleisli (landLeft 2) >>> Kleisli (landRight 2)) (0, 0)
  print $ runKleisli (Kleisli (landRight 1) >>> Kleisli (landLeft 4) >>> Kleisli (landRight (-1)) >>> Kleisli (landRight 2)) (0, 0)
  print $ runKleisli (Kleisli (landLeft 1) >>> Kleisli banana >>> Kleisli (landRight 1)) (0, 0)

-- Arrow の定義を追加
instance Arrow NonDet where
    arr f = NonDet (return . f)
    first (NonDet f) = NonDet (\(x, z) -> f x >>= (\y -> return (y, z)))

main :: IO ()
main = do
  putStrLn "end"
