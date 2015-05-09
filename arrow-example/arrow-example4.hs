{-# LANGUAGE Arrows #-}
import qualified Control.Category as Cat
import Control.Arrow
import MyDebug

-- Arrow のクラス群を実装したサンプル

newtype SF b c = SF {runSF :: [b] -> [c]}
instance Cat.Category SF where
  id = SF Cat.id
  SF f . SF g = SF (f Cat.. g)
  
instance Arrow SF where
  arr f = SF (map f)
  first (SF f) = SF (unzip >>> first f >>> uncurry zip)

instance ArrowChoice SF where
  left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
    where
      combine (Left  y:xs) (z:zs) = Left  z: combine xs zs
      combine (Right y:xs) zs     = Right y: combine xs zs
      combine []           zs     = []  

instance ArrowLoop SF where
    loop (SF f) = SF $ \as ->
        let (bs,cs) = unzip $ f $ zip as $ stream cs
        in  bs
      where stream ~(x:xs) = x:stream xs

instance ArrowApply SF where
  app = SF $ \xs -> let (fs, ys) = unzip xs
                        (SF f)   = head fs
                    in  f ys

delay :: c -> SF c c
delay x = SF (init . (x:))
delay' :: c -> SF c c
delay' x = SF (x:)
odd' :: Integral b => b -> Either b b
odd' x = if odd x then Left x else Right x

mapA :: (ArrowChoice a) => a b c -> a [b] [c]
mapA f = arr listcase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:))) 
    where
      listcase []     = Left  ()
      listcase (x:xs) = Right (x,xs)

nor :: SF (Bool,Bool) Bool
nor = arr (not . uncurry (||))

edge :: SF Bool Bool
edge  = edgeA >>> edgeB
edgeA = arr id &&& delay False
edgeB = arr f
  where f (a,b) = a && not b
debugEdge = do
    print $ runSF (edgeA)           input
    print $ runSF (edgeA >>> edgeB) input
  where
    input  = [True,False,True,False]
-- doを使って記述
edge' = proc a -> do
    b <- delay False -< a
    returnA -< a && not b

flipflop = loop $ ffA >>> ffB >>> ffC >>> ffD
ffA = arr f
  where f ((reset,set), ~(c,d)) = ((set,d), (reset, c))
ffB = nor *** nor
ffC = delay (False, True)
ffD = arr id &&& arr id
debugFF = do
    print $ runSF (ffA) input
    print $ runSF (ffA >>> ffB) input
    print $ runSF (ffA >>> ffB >>> ffC) input
    print $ runSF (ffA >>> ffB >>> ffC >>> ffD) input
  where
    input = [((True,False),(True,False))]
-- loop は rec になる
-- ArrowLoop を実装していないと使えない
flipflop' = proc (reset,set) -> do
    rec
      c <- delay False -< nor reset d
      d <- delay True  -< nor set c
    returnA -< (c,d)
    where nor a b = not (a || b )

pp :: SF (Bool, Bool) Bool
pp = proc (x,y) -> do
    delay False -< x && y
-- 複雑な受け渡し
halfAdd :: Arrow a => a (Bool,Bool) (Bool,Bool)
halfAdd = proc (x,y) -> do
    returnA -< (x && y, x /= y)
fullAdd :: Arrow a => a (Bool,Bool,Bool) (Bool,Bool)
fullAdd = proc (x,y,c) -> do
    (c1,s1) <- halfAdd -< (x, y)
    (c2,s2) <- halfAdd -< (s1,c)
    returnA -< (c1 || c2, s2)

-- proc の中で &&& が使える ||| は使えない
example :: SF Int (Int, Int)
example = proc x ->
    do returnA -< x
    &&& 
    do delay 0 -< x

-- (| |) バナナ括弧を使うと前置することができる
exampleA = proc x -> (| (&&&) (returnA -< x) (delay 0 -< x) |)

mapC :: ArrowChoice arr => arr (env,a) b -> arr (env,[a]) [b]
mapC c = proc (env,xs) ->
  case xs of
    []    -> returnA -< []
    x:xs' -> do 
        y  <- c      -< (env,x)
        ys <- mapC c -< (env,xs')
        returnA -< y:ys

exampleB = proc (n,xs) -> 
           (| mapC (\x -> (do delay 0 -< n) &&& (do returnA -< x)) |) xs

exx = do
  print $ runSF example  [1..5]
  print $ runSF exampleA [1..5]


main :: IO ()
main = do
  print $ runSF (arr (+1)) [1..5]
  print $ flip runSF [1,2] $ proc x -> do
    returnA -< (x+1)
  print $ runSF (left (arr (*2))) [Left 1, Right 2, Left 3]
  print $ runSF (arr odd' >>> arr (*3) ||| arr (*10)) [1..5]
  print $ runSF (delay' 0) [1,2]
  print $ runSF (mapA (delay' 0)) [[1,2,3],[4,5],[6],[7,8],[9,10,11],[12,13,14,15]]
  print $ runSF pp [(True,True)]
  print $ runSF fullAdd [(True,True,False),(True,False,False)]
  print $ runSF (delay 0)  [1..5]
  print $ runSF (app(delay,0)) [1..5]
  print $ flip runSF [1..5] $ proc x -> do
    delay 0 -< x
  print $ flip runSF [1..5] $ proc x -> do
    delay 0 -<< x
