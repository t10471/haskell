
import Control.Applicative
import Data.Monoid
import Control.Monad

up :: Int -> Int
up = (+) 1

-- Applicative
-- 関数の中で計算する
-- モナドとは関数部分が異なる
-- (>>=) :: Monad m => m a -> (a -> m b ) -> m b
-- (<*>) :: f (a -> b ) -> f a -> f b
--
-- pure Applicative に包んで返す
apP :: Applicative f =>  Int -> f Int 
apP  = pure
-- <*> Applicative の中で計算する
apL :: Applicative f =>  (Int -> Int) -> Int -> f Int 
apL f i = pure f <*> pure i
-- *> 左側の値を無視する
apLR :: Applicative f =>  Int -> Int -> f Int 
apLR i j = pure i *> pure j
-- <* 右側の値を無視する
apLL :: Applicative f =>  Int -> Int -> f Int 
apLL i j = pure i <* pure j

exAp :: IO ()
exAp = do
    print $ (apP 1 :: [Int])
    print $ (apL up 1 :: [Int])
    print $ (apLR 1 2 :: [Int])
    print $ (apLL 1 2 :: [Int])

-- Alternative
-- MonadPlus の Applicative 版
-- こっちを使った方がいいらしい

retEp :: Alternative f => Int -> f Int
retEp 0 = empty
retEp i = pure i 

retEpl :: [Int] -> [Int]
retEpl (x:xs) | x /= 0 = x : retEpl xs
retEpl _ = []

-- empty Applicative の Monoid の 単位元
alE :: Alternative f =>  f a
alE = empty
-- 単位元ではない方を返す List は連結になる
alOr :: Alternative f => Int -> Int -> f Int
alOr i j = retEp i <|> retEp j

-- some と many が存在するが、
-- List Maybe では 無限ループするので、
-- applicative-example2.hs に独自クラスを定義

-- zip 関数とは違い長さに関係なく連結できる
exZipList :: IO ()
exZipList = do
    print $ getZipList $ (,,) <$> ZipList "abc" <*> ZipList [1,2,3] <*> ZipList "def"
    print $ getZipList $ ZipList [(*), (+)] <*> ZipList [1,2] <*> ZipList [3,4]

-- ユーティリティー
-- <$> は大事
exB :: IO ()
exB = do
  -- 3つはおなじ
  print $ [(+1),(+2)] <*> [2,3]
  print $ (+) <$> [1,2] <*> [2,3]
  print $ [2,3] <**> [(+1),(+2)]
  -- 中身は第一引数、型は第二引数にする
  print $ 1 <$ [2,3] 
  -- 第二引数の型に関数を適用する
  print $ liftA (+1) [1,2]
  print $ liftA2 (+) [1,2] [2,3]

  -- Maybe 型をふかしてくれるような・・・?
  print $ optional ([] :: [Int])
  -- [Nothing]
  print $ optional [1]
  -- [Just 1,Nothing]
  print $ optional [1,2]
  -- [Just 1,Just 2,Nothing]
  print $ optional $ Just 1
  -- Just (Just 1)


exAl :: IO ()
exAl = do
    print $ (alE :: [Int])
    print $ (alOr 1 2 :: [Int])
    print $ (alOr 0 0 :: Maybe Int)
    print $ (alOr 0 2 :: Maybe Int)
    print $ (alOr 1 0 :: Maybe Int)

main :: IO ()
main = do
  putStrLn "end"
