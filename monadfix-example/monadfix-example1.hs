
import Control.Monad.Fix


-- fix mfix の定義
-- fx::(a->a)->a
-- fix f = let x = f x in x
--

-- シンプルな不動点
fact :: Int -> Int
   --  fix :: ((Int -> Int) -> Int -> Int) -> Int -> Int
   --    g ::  (Int -> Int) -> Int -> Int
fact = fix g
    where  
           g f n | n==0      = 1
                 | otherwise = n* f (n-1) 

-- fix :: (([a] -> a) -> [a] -> a) -> [a]
--   g ::  ([a] -> a) -> [a] -> a
findmax :: Ord a => [a] -> a
findmax = fix g 
   where g f ns | ns == []      = error "Null List!"
                | tail ns == [] = head ns
                | otherwise     = max (head ns) (f (tail ns))

-- maxも引数にする
--  fix :: (([a] -> a) -> [a] -> a) -> [a] -> a
--    g ::  ([a] -> a) -> [a] -> a
find' :: Ord a => (a -> a -> a) -> [a] -> a
find' h = fix g 
   where g f ns | ns == []     = error "Null List!" 
                | tail ns ==[] = head ns
                | otherwise    = h (head ns) (f (tail ns))

-- fix :: ((a -> a) -> a -> a) -> a -> a 
--   g ::  (a -> a) -> a -> a
fib = fix g
    where g f n | n <= 0      = 0
                | n == 1      = 1
                | otherwise   = f (n-1) + f (n-2)

-- 相互再帰 Version
-- ~() は遅延評価
-- fix :: ((Integer -> Bool, Integer -> Bool) -> (Integer -> Bool, Integer -> Bool)) -> (Integer -> Bool, Integer -> Bool)
--   g ::  (Integer -> Bool, Integer -> Bool) -> (Integer -> Bool, Integer -> Bool)
(isodd, iseven) = fix g
  where g  ~(o,e) = (\n -> if (n == 0) then False else e (n - (signum n)),
                     \n -> if (n == 0) then True  else o (n - (signum n)))

-- 複雑 Version
data Tree' a = L' a | N' (Tree' a) (Tree' a) deriving Show

copy:: Tree' Int -> Int -> (Tree' Int, Int)
copy = fix g
    where
        g:: (Tree' Int -> Int -> (Tree' Int, Int))-> (Tree' Int -> Int -> (Tree' Int, Int))
        g cp = \ t m  -> case t of
                          (L' a)   -> (L' m, a)
                          (N' l r) -> let (l',ml) = cp l m
                                          (r',mr) = cp r m
                                      in (N' l' r', ml `min` mr)

-- fst.(copy t) :: Int -> Tree' Int
--          fix :: (Int -> Int) -> Int
-- snd.(copy t) ::  Int -> Int
repmin :: Tree' Int -> Tree' Int
repmin t = (fst.(copy t)) (fix $ (snd.(copy t)) )

-- mfix::(a->[a])->[a]
-- mfix f = case fix(f.head) of
--             []   -> []
--             x:_ -> x:mfix (tail.f)
-- MonadFix の性質
-- purity
-- mfix (return . h) = return (fix h)
-- left shrinking (or tightening)
-- mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y) 
-- sliding
-- mfix (liftM h . f) = liftM h (mfix (f . h)), for strict h. 
-- nesting
-- mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)

data Tree = L Int | N Tree Tree deriving Show

replace :: Int -> Tree-> [(Tree,Int)]
replace x (L y) = [(L x,y)]
replace x (N l r) = [(N l' r, y) | (l',y) <- replace x l ]
                    ++ 
                    [(N l r', y) | (r',y) <- replace x r ]

pairSwaps :: Tree -> [Tree]
pairSwaps e = (mfix  f)  >>= \(e'',n) -> return e''
              where f  ~(e'', n) = do (e', m) <- replace n e 
                                      (e'',n) <- replace m e' 
                                      return (e'',n)


main :: IO ()
main = do
  let l = [1, 4, 2, 4, 6, 3]
  print $ fact 4
  print $ findmax l
  print $ find' max l
  print $ fib 4
  print $ isodd 5
  print $ repmin (N' (L' 2) (N' (L' 5) (N' (N' (L' 10) (L' 9)) (L' 3)))) 
  print $ pairSwaps (N (L 1 ) (L 2 ) )

