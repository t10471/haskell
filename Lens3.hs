data User = User { name :: String, age :: Int } deriving Show
-- nameは関数 name :: User -> String
data Project = Project { owner :: User } deriving Show

data NaiveLens s a = NaiveLens
                         { view :: s -> a
                         , over :: (a -> a) -> s -> s }

set :: NaiveLens s a -> a -> s -> s
set ln a s = over ln (const a) s
-- const :: a -> b -> a
-- constは第一引数をそのまま返す
-- id :: a -> a
-- const id :: b -> a -> a
-- const  1 2 は1
-- const id 1 2 は2
-- const id 1 を評価してidを返し、id 2で2をかえす


ageLens :: NaiveLens User Int
ageLens = NaiveLens age (\f s -> s { age = f (age s) })

main = do let john = User { name = "John", age = 30 }
          print $ over ageLens (+1) john
          print $ set ageLens 1 john

