{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables #-}

import Control.Monad.Free

data Hoge a = Foo a | Bar a deriving Show

data Yoneda f x = Yoneda { runYoneda :: forall b. (x -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f m = Yoneda $ \k -> runYoneda m (k . f)

yo = Yoneda (\k -> Foo (k 5))
{-
YonedaのFunctorは関数の入力を書き換えることができる
              m :: Yoneda f x
fmap (x -> y) m :: Yoneda f y

runYoneda                 m :: (x -> b) -> f b 
runYoneda $ fmap (x -> y) m :: (y -> b) -> f b

fmapの簡約手順
runYoneda (fmap succ               (Yoneda (\k1 -> Foo (k1 5))))                 show
runYoneda (Yoneda (\k -> runYoneda (Yoneda (\k1 -> Foo (k1 5))) (k    . succ ))) show
runYoneda                          (Yoneda (\k1 -> Foo (k1 5))) (show . succ)
Foo ((show . succ) 5)

Yonedaの双対のCoYonedaは入力と出力が逆になる

runYoneda         runCoYoneda
(x -> b ) -> f b  f b -> (b -> x )

CoYonedaのfmapは、関数の出力を書き換える

              m :: CoYoneda1 f x
fmap (x -> y) m :: CoYoneda1 f y

runCoYoneda1          m :: f b -> (b -> x )
runCoYoneda1 $ fmap k m :: f b -> (b -> y )
-}

data CoYoneda1 f x = CoYoneda1 { runCoYoneda1 :: forall b. f b -> (b -> x )  }

instance Functor (CoYoneda1 f) where
    fmap f (CoYoneda1 g) = CoYoneda1 $ \x d -> f $ g x d

-- aとbの型を指定指定できないので使い物にならない・・・
co1 = CoYoneda1 $ \(Foo a) -> \b -> 1
{-

f b -> (b -> x )を (->) f b (b -> x) と考え
この、(->)をデータコンストラクタCoYonedaに置き換える

CoYoneda (f b) (b -> x)

-}

data CoYoneda2 f x = forall b. CoYoneda2 (f b) (b -> x)

instance Functor (CoYoneda2 f) where
    fmap f (CoYoneda2 v g) = CoYoneda2 v (f . g)

-- 任意の型をCoyonedaにできる
liftCoYoneda2 :: f a -> CoYoneda2 f a
liftCoYoneda2 x = CoYoneda2 x id

co2 = CoYoneda2 (Foo 5) (\b -> succ b)

-- 順序を変えた方が使いやすいので逆にする

data CoYoneda f x = forall b. CoYoneda (b -> x) (f b)

instance Functor (CoYoneda f) where
    fmap f (CoYoneda g v) = CoYoneda (f . g) v

liftCoYoneda :: f a -> CoYoneda f a
liftCoYoneda = CoYoneda id

-- Coyonedaからデータを取り出すのにFreeモナドが必要

type Program f a = Free (CoYoneda f) a
data MyPg a = Order1 a | Order2 a

type MyProgram a = Program MyPg a

singleton :: f a -> Program f a
singleton = liftF . liftCoYoneda

order1 = singleton $ Order1 () :: MyProgram ()
order2 = singleton $ Order2 () :: MyProgram ()

runMyPg :: MyProgram a -> IO a
runMyPg (Free (CoYoneda f o)) = runOrder o >>= runMyPg . f
  where
    runOrder :: MyPg a -> IO a
    runOrder (Order1 n) = putStrLn "Order1 Called!!" >> return n
    runOrder (Order2 n) = putStrLn "Order2 Called!!" >> return n
runMyPg (Pure a) = return a

program :: MyProgram ()
program = do
  order1
  order2
  order2
  order1

main :: IO ()
main = runMyPg program

