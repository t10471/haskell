--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--
-- [1,2,3] `minus` [1]   => [2,3]
-- [1,2,3] `minus` [1,2] => [3]
-- [1,2,3] `minus` [2]   => [1,3]

module MyList (minus) where

minus                 :: (Eq x) => [x] -> [x] -> [x]
xs `minus` ys         =  foldl rmv xs ys
rmv                   :: (Eq x) => [x] -> x -> [x]
[] `rmv` y            =  []
(x:xs) `rmv` y        =  if  x == y  then  xs  else  x : (xs `rmv` y)
