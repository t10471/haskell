{-# LANGUAGE Arrows #-}

import Control.Arrow

-- Kleisli の基本
-- モナドを包んだ Arrow
-- runkleisli で実行

up :: Int -> Int
up = (+1)
upL :: Int -> [Int]
upL = return . up
down :: Int -> Int
down = flip (-) 1

-- IO モナドの関数を Kleisli で包むと Arrow とつなぐことができる
countW :: String -> Kleisli IO String ()
countW w = Kleisli readFile   >>> arr words           >>> 
           arr (filter (==w)) >>> arr (show . length) >>> 
           Kleisli putStrLn
exCountW :: IO ()
exCountW = runKleisli (countW "a") "arrow-example1.hs"

-- Kleisli を使ってモナドをつなぐ
exArr :: (Monad m) => (a -> b1 ) -> (b1 -> m b2) -> (b2 -> m b) -> a -> m b
exArr f g h a = runKleisli (arr f >>> Kleisli g >>> Kleisli h) a

exK :: (Monad m) => Kleisli m a b -> a -> m b
exK k x = runKleisli k x

arrZ :: Int ->  Kleisli [] Int Int
arrZ i = case i of
          0         -> zeroArrow
          otherwise -> Kleisli (\x -> return (x + i))

arrP :: Int -> Int -> Kleisli [] Int Int
arrP i j = (arrZ i) <+> (arrZ j)

xx :: IO ()
xx = do
  print $ runKleisli (app (arrZ, 1)) 1

addM :: Monad m => m Int -> m Int -> m Int
addM a b = do 
    a' <- a
    b' <- b
    return (a' + b')

addA a b = (arr (\v -> v) >>> a) `bind`
           ( 
             (arr (\(v, a') -> v) >>> b) `bind` 
              arr (\((v, a'), b') -> (a' + b'))
           )
bind :: Arrow a => a b c -> a (b,c) d -> a b d
u `bind` f = arr id &&& u >>> f

arrM :: IO ()
arrM =  return (addM (ArrowMonad (Kleisli (\_->Just 10))) (ArrowMonad (Kleisli (\_->Just 32)))) >>
        return (addA (Kleisli (\_->(ArrowMonad (Kleisli (\_->Just 10))))) (Kleisli (\_->(ArrowMonad (Kleisli (\_->Just 32)))))) >>
        return (addM (ArrowMonad (Kleisli (\_->(ArrowMonad (Kleisli (\_->Just 10)))))) (ArrowMonad (Kleisli (\_->(ArrowMonad (Kleisli (\_->Just 32))))))) >>
        return ()

mainF :: IO ()
mainF = do
    print $ exArr up upL upL 1

main :: IO ()
main = do
    putStrLn "end"

