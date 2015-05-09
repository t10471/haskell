
data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

setOwnerName :: String -> Project -> Project
setOwnerName newName p = p { owner = (owner p) { name = newName } }


main = do let bob = User { name = "Bob", age = 30 }
          let project = Project { owner = bob }
          print project
          print $ setOwnerName "Alice" project
