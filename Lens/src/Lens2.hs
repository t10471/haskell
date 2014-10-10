
data User = User { name :: String, age :: Int } deriving Show
-- nameは関数 name :: User -> String
data Project = Project { owner :: User } deriving Show

data NaiveLens s a = NaiveLens
                         { view :: s -> a
                         , set  :: a -> s -> s }
-- NaiveLens :: (s -> a) -> (a -> s -> s) -> NaiveLens s a
nameLens :: NaiveLens User String
nameLens = NaiveLens name (\a s -> s { name = a })
-- nameが s -> aになっている
--set :: NaiveLens s a -> a -> s -> s
--set nameLens :: String -> User -> User

main = do let john = User { name = "John", age = 30 }
          print $ set nameLens "Bob" john
