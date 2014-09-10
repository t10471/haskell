{-
fmapの定義
class Functor f 
    where fmap :: (a1 -> b1) -> f a1 -> f b1

f = Pair1 c
なので
fmap :: (a1 -> b1) -> Pair1 c a1 -> Pair1 c b1
となる

(a1 -> b1) -> Pair1 c a1 -> Pair1 c b1
は第一引数(c)は変わらず第二引数がa1からb1に変わることを示している
よってPair1 a bはbの値が変化するということを許可している

逆にPair2 b aというのはbの値が変化しないということである

Pair1 :: (a, b) -> Pair1 a b
タプル以外も渡せるがgetPair1時にエラーになる
Pair1 1 :: Num (a, b) => Pair1 a b
-}

newtype Pair1 a b = Pair1 { getPair1 :: (a, b) } 

instance Functor (Pair1 c) 
    where fmap f (Pair1 (x, y)) = Pair1 (x, f y) 

newtype Pair2 b a = Pair2 { getPair2 :: (a, b) } 

instance Functor (Pair2 c) 
    where fmap f (Pair2 (x, y)) = Pair2 (f x, y) 

main = do
          let p1 = Pair1 (1, 2)
          let p2 = Pair2 (1, 2)
          print $ getPair1 $ fmap (+1) p1
          print $ getPair2 $ fmap (+1) p2
          
