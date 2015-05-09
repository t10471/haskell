
data Group a = Group {groupUnnamed :: [a], groupNamed :: [(String, [a])]}
  deriving Show

instance Functor Group where
    fmap f (Group a b) = Group (map f a) [(x, map f y) | (x,y) <- b]

main :: IO ()
main = do
  let g = Group {groupUnnamed = ["a"], groupNamed = [("x",["s"])]}
  print $ fmap (\x -> x ++ x) g 
