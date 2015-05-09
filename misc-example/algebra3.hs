import Data.List

-- catamorphisms
-- foldの一般化
cata :: b -> (a -> b -> b) -> [a] -> b
cata b f []     = b
cata b f (a:as) = f a (cata b f as)
 
cataLength :: [a] -> Int
cataLength = cata 0 (<+>)
  where a <+> n = 1 + n
 
cataFilter p = cata [] (<+>)
  where a <+> as
          | p a       = a:as
          | otherwise = as
 
-- anamorphisms
-- unfoldの一般化
ana :: (b -> (a, b)) -> (b -> Bool) -> b -> [a]
ana g p b
  | p b       = []
  | otherwise = a:ana g p b'
  where (a, b') = g b
 
anaZip :: [a] -> [b] -> [(a, b)]
anaZip = curry (ana g p)
  where p ([], _) = True
        p (_, []) = True
        p _       = False
        g ((a:as), (b:bs)) = ((a, b), (as, bs))
 
anaIterate :: (a -> a) -> a -> [a]
anaIterate f = ana g (const False)
  where g a = (a, f a)
 
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (a:as) = f a:map' f as
 
cataMap :: (a -> b) -> [a] -> [b]
cataMap f = cata [] (<+>)
  where a <+> bs = (f a):bs
 
anaMap :: (a -> b) -> [a] -> [b]
anaMap f = ana g p
  where p []     = True
        p _      = False
        g (a:as) = (f a, as)
 
-- Hylomorphisms
-- cataとanaの合成
hylo c f g p a
  | p a       = c
  | otherwise = f b (hylo c f g p a')
  where (b, a') = g a
 
fac :: Int -> Int
fac 0  = 1
fac n  = n * fac (n - 1)
                  
hyloFac :: Int -> Int
hyloFac = hylo 1 (*) g p
  where p n = n == 0
        g n = (n, n - 1)
 
-- Paramorphisms
-- hyloの拡張
numPara b f 0 = b
numPara b f n = f (n - 1) (numPara b f (n - 1))
 
listPara b f []     = b
listPara b f (a:as) = f a (as, listPara b f as)
 
paraFac :: Int -> Int
paraFac = numPara 1 f
  where f n m = (1 + n) * m
 
paraTails :: [a] -> [[a]]
paraTails = listPara ([]:[]) f
  where f a (as, tls) = (a:as):tls

main :: IO ()
main = do
  putStrLn "end"
