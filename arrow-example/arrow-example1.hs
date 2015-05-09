{-# LANGUAGE Arrows #-}

import Control.Arrow

-- Arrow の基本

up :: Int -> Int
up = (+1)
upL :: Int -> [Int]
upL = return . up
down :: Int -> Int
down = flip (-) 1

-- arr は 関数を Arrow に包んで返す
toArr :: (Arrow a) => (Int -> Int) -> a Int Int
toArr = arr
-- >>> は Arrow をつなぐ
toString :: (Arrow a) => (Int -> Int) -> a Int String
toString f = arr f >>> arr show
-- 関数と Arrow をつなぐ
arrJoin :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a Int Int
arrJoin f g = f ^>> arr g
-- 逆
arrJoir :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a Int Int
arrJoir f g = arr f >>^ g
-- -> は Arrowの インスタンス
arrACont :: (->) [Int] Int
arrACont = filter odd >>> filter (> 10) >>> length
-- first はタプルを受け取るが1つめの値しか計算しない
arrFst :: (Arrow a) => (Int -> Int) -> a (Int, d) (Int, d)
arrFst = first . arr 
-- second はタプルを受け取るが2つめのの値しか計算しない
arrScd :: (Arrow a) => (Int -> Int) -> a (c, Int) (c, Int)
arrScd = second . arr
-- *** は 2つの関数を1つにする
arrCn :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a (Int, Int) (Int, Int)
arrCn f g = arr f *** arr g
-- &&& は 2つの関数の結果を1つにする
arrCr :: (Arrow a) => (Int -> Int) -> (Int -> Int) -> a Int (Int, Int)
arrCr f g = arr f &&& arr g
arrACr :: (->) [Int] Int
arrACr = filter odd >>> (
            (filter (> 10) >>> length >>> (*10)) &&& 
            length
          ) >>> uncurry (+)

mainA :: IO ()
mainA = do
    print $ toArr up $ 1
    print $ toString up $ 1
    print $ arrJoin up down $ 1
    print $ arrJoir up down $ 1
    print $ arrFst up $ (1,2)
    print $ arrScd up $ (1,2)
    print $ arrCn up down $ (1,2)
    print $ arrCr up down $ 1

-- ArrowChoice
-- 引数が Rightのときは計算を行い Left のときはそのまま値を返す
arrCR :: (ArrowChoice a) => (Int -> Int) -> a (Either d Int) (Either d Int)
arrCR = right . arr
-- 引数が Left のときは計算を行い Right のときはそのまま値を返す
arrCL :: (ArrowChoice a) => (Int -> Int) -> a (Either Int d) (Either Int d)
arrCL = left . arr
-- 関数を2つ保持し引数に応じて処理を行う
arrCAnd :: (ArrowChoice a) => (Int -> Int) -> (Int -> Int) -> a (Either Int Int) (Either Int Int)
arrCAnd f g = (arr f) +++ (arr g)
-- 関数を2つ保持し引数に応じて処理を行う 結果は Either にくるまれていない
arrCOr :: (ArrowChoice a) => (Int -> Int) -> (Int -> Int) -> a (Either Int Int) Int
arrCOr f g = (arr f) ||| (arr g)
arrCCOr :: (->) [Int] Int 
arrCCOr =  hasOver20 >>> (filter(> 10) ||| filter odd) >>> length 
    where hasOver20 ns | any (> 20) ns = Right ns 
                       | otherwise     = Left ns

-- map の Arrow 版
mapA f = arr listcase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:))) 
         where
          listcase []     = Left  ()
          listcase (x:xs) = Right (x,xs)


exC :: Show s => String -> (Either b b -> s) -> b -> IO ()
exC s f i = do
  ex s f $ Left i
  ex s f $ Right i
  where ex s f ei = do putStr (s ++ " "); print $ f ei 
mainB :: IO ()
mainB = do
    exC "arrCR"   (arrCR up) 1
    exC "arrCL"   (arrCL up) 1
    exC "arrCAnd" (arrCAnd up down) 1
    exC "arrCOr"  (arrCOr  up down) 1


-- ArrowLoop
-- MonadFix の Arrow版
-- loop 関数のみ
-- loop の仕組みにつてい
-- 下記が実装
loop' f b = let (c,d) = f (b,d) in c
-- loop' (\(x,y) -> (y+1,x+2)) 3 のサンプル
-- (c,d) = (d+1,b+2)
-- c = d+1  d = b+2 なので
-- b = 3 をいれると
-- c = (3+2)+1 となり 結果6となる
-- ループする奴
-- タプルの2番目を伸ばすことにより終わらなくなる
exRepeat :: Int -> a -> [a]
exRepeat i j = take i $ loop (snd &&& uncurry(:)) $ j

decWithMinA :: [Int] -> [Int]
decWithMinA ls = (loop _decWithMinA) ls
  where
    _decWithMinA ([x]   , m) = ([x - m]      , x       )
    _decWithMinA ((x:xs), m) = ((x - m) : xs', min x m')
      where (xs', m') = _decWithMinA (xs, m)

-- ArrowApply
-- Arrow を引数に取れる
arrAApp :: (Int -> Int) -> Int -> Int
arrAApp f i = app (arr f, i)

addM :: Monad m => m Int -> m Int -> m Int
addM a b = do 
    a' <- a
    b' <- b
    return (a' + b')
-- 計算できるが中身は取り出せない
arrAAM :: ArrowMonad (->) Int
arrAAM = do
    let x = ArrowMonad (arr (\_->  1))
    let y = ArrowMonad (arr (\_->  2))
    let z = addM x y
    zz <- z
    return zz



main :: IO ()
main = do
    return arrAAM >> putStrLn "ArrowMonad"
    putStrLn "end"

