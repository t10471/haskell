import Control.Lens

-- 本物のLensを使用したversion

data User = User String [Post] deriving Show
data Post = Post String deriving Show

posts :: Lens' User [Post]
posts f (User n p) = fmap (\p' -> User n p') (f p)

title :: Lens' Post String
title f (Post t) = fmap Post (f t)

users :: [User]
users = [User "john" [Post "hello", Post "world"], User "bob" [Post "foobar"]]

main :: IO ()
main = do
    print $ view (traverse.posts) users
    print $ view (traverse.posts.traverse.title) users
