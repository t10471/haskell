
import Data.Monoid
import Data.Function (on)
import Control.Applicative

{-
モノイドとは
  1.台集合 M
  2.二項演算 ⋅ : M × M → M  
    結合律を満たす
  3.単位元 e
の組(M,⋅,e)のこと。

単位元を除くと半群(semigroup)、逆元の存在を加えると群(group)になる。
-}

base = do
  -- mappend は <> と等しい
  print $ "a" <> "b"
  print $ "a" <> mempty

math = do
  print $ getSum $ Sum 3 <> Sum 4
  print $ getProduct $ Product 3 <> Product 4
  print $ getSum mempty
  print $ getProduct mempty
  print $ getSum $ mconcat [Sum 3, Sum 4]

bool = do
  print $ getAll $ All True <> All True
  print $ getAll $ All True <> All False
  print $ getAny $ Any True <> Any True
  print $ getAny $ Any True <> Any False
  print $ getAll $ mconcat [All True, All False]

order = do
  print $ mconcat [EQ, EQ, EQ, GT, EQ, LT]
  print $ zipWith compare "aabc" "abcd"
  -- on は (b -> b -> c ) -> (a -> b ) -> a -> a -> c で compare
  -- の ソート関数を指定するのによく使う
  let f x y = mconcat (zipWith compare x y) <> (compare `on` length) x y 
    in print $ f "aabc" "abcd"

(&) :: a -> (a -> b ) -> b
x & f = f x
infixl 1 &

ed = do
  -- 入力と出力が同じ関数は合成出来る
  -- 後ろから計算される
  print $ Endo (+2) <> Endo (*2) & appEndo $ 0
  print $ (appEndo . mconcat . map Endo) [(+ 2 ), (* 2 )] 0
  print $ Endo (*2) <> Endo (+2) & appEndo $ 0
  print $ (appEndo . mconcat . map Endo) [(* 2 ), (+ 2 )] 0
  print $ (appEndo . mconcat . map Endo) [(+ 1 ), (* 4 ), (+ 3 )] 0
  -- 順序を逆にする
  print $ getDual $ Dual "foo" <> Dual "bar"
  print $ (appEndo . getDual . mconcat . map (Dual . Endo)) [(+ 1 ), (* 4 ), (+ 3 )] 0
  -- 自明なモノイド
  print $ () <> ()
  -- (,)もモノイド
  print $ (Sum 3, Product 3) <> (Sum 4, Product 4)
  -- 常に先頭を取得 左自明モノイド
  print $ First (Just 'A') <> First (Just 'B')
  -- 常に最後を取得 右自明モノイド
  print $ (getLast . mconcat . map Last) [Just 'A', Nothing, Just 'B', Nothing, Nothing]
  -- リストは自由モノイド 結合則以外に法則が存在しないモノイドのこと(数字には乗法がある 2 * 3 = 6)
  print $ mempty <> [1,2]


-- モノイドをモナドにする
-- Writerモナドと同一のもの
-- つまりモノイドに乗法(>>=)を追加するとモナドになる
data Stamp m a = Stamp a m

instance Monoid m => Functor (Stamp m) where
  f `fmap` Stamp a m = Stamp (f a) m 

instance Monoid m => Applicative (Stamp m) where
  pure a                  = Stamp a mempty
  Stamp f m <*> Stamp a n = Stamp (f a) (m <> n)

instance Monoid m => Monad (Stamp m) where
  return a        = Stamp a mempty
  Stamp a m >>= f = let Stamp b n = f a in Stamp b (m <> n)

stamp :: Monoid m => m -> Stamp m ()
stamp = Stamp ()

runStamp :: Stamp m a -> (a, m)
runStamp (Stamp a m) = (a, m)

stp = do
  print $ runStamp $ do { stamp "foo" ; (do { stamp "bar" ; return "baz"  }) >>= stamp  }
  print $ runStamp $ do { stamp (First (Just 'a')) ; stamp (First (Just 'b')) ; return 'c' }
  print $ runStamp $ do { stamp (Last (Just 'a')) ; stamp (Last (Just 'b')) ; return 'c' }
  print $ runStamp $ do { stamp () ; stamp () ; return 42  }







main :: IO ()
main = do
  putStrLn "end"
