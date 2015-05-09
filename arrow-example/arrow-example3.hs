{-# LANGUAGE Arrows #-}

import Control.Arrow

-- Arrow の do構文

-- addA addA' は同じ
addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = f &&& g >>> arr (\ (y, z) -> y + z )

addA' :: Arrow a => a b Int -> a b Int -> a b Int
addA' f g = proc x -> do
    y <- f -< x
    z <- g -< x
    returnA -< y + z

idA :: a -> a
idA = proc a -> returnA -< a

plusOne :: Int -> Int
plusOne = proc a -> returnA -< (a+1)

plusTwoBis :: Int -> Int
plusTwoBis =  proc a -> do 
    b <- plusOne -< a
    plusOne -< b

-- proc pat -> a -< e 
-- 同じ
-- arr (\pat -> e) >>> a


-- proc 由来の変数は左側では使えず
-- 常に右からしか使えない
xx = proc (f,x) -> do
     app -< (f,x)
-- 上と同じことが -<< でできる
-- -<< は ArrowApply を実装していないと使えない
xx' = proc (f,x) -> do
     f -<< x

xxy  = arr (\x -> if odd x then Left x else Right x) >>> return "Odd" ||| return "Even"
-- 上と同じ
xxy' = proc x -> do
    if odd x then 
        return "Odd" -< x 
    else 
        return "Even" -< x

mapA  f = arr listcase >>>
          arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
  where listcase []     = Left  ()
        listcase (x:xs) = Right (x,xs)
-- 上と同じ
-- returnA -< [] は
-- arr (const []) -< xs
-- とおなじ
-- case や if を使うには ArrowChoice を実装していなければいけない
mapAA f = proc xs ->
  case xs of
    []    -> returnA                        -< []
    x:xs' -> (f *** mapA f >>> uncurry (:)) -< (x,xs')
-- さらに do記法を使った場合
mapAB f = proc xs ->
  case xs of
    []    -> returnA -< []
    x:xs' -> do
      y   <- f      -< x
      ys' <- mapA f -< xs'
      returnA       -< y:ys'

ex = do
  print $ xx  ((*2),3)
  print $ xx' ((*2),3)
  print $ xxy  6
  print $ xxy' 6
  print $ mapA  (+1) [1,2]
  print $ mapAA (+1) [1,2]
  print $ mapAB (+1) [1,2]

printFile = proc name ->　do
    s <- Kleisli readFile -< name
    Kleisli print -< s

zz :: [(b -> c,b)] -> [c]
zz []         = []
zz ((f,x):xs) = (f x) : (zz xs)

main :: IO ()
main = do
    putStrLn "end"

