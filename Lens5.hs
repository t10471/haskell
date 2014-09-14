{-# LANGUAGE RankNTypes #-}

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Traversable (traverse)

{- 
type Lens s   a   = forall f. Functor f => (a -> f a) -> s -> f s 
   これを下に変える
   fmapに類似している
Functor f => (a -> b) -> f a -> f b
   実は以下の関係がある
type Lens' s a = Lens s s a a
-}
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- 本当のLensはFunctorだけど、Traverableを使うためにApplicativeに変更する
-- 本当のやつはもっと複雑なのでFunctorでOK？
type Lens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

data User = User String [Post] deriving Show
data Post = Post String deriving Show

{-
Lens'はLens s s a a なので
Lens User User [Post] [Post]
Lens は Functor f => (a -> f b) -> s -> f t　なので
結局こうなる
Functor f => ([Post] -> f [Post]) -> User -> f User
-}
posts :: Lens' User [Post]
posts f (User n p) = fmap (\p' -> User n p') (f p)
{-
traverse       :: (Traversable t, Applicative f) => (a      -> f b     ) -> t a    -> f (t b)
posts          ::                     Functor f  => ([Post] -> f [Post]) ->   User -> f    User
traverse.posts :: (Traversable t, Applicative f) => ([Post] -> f [Post]) -> t User -> f (t User)
-}
title :: Lens' Post String
title f (Post t) = fmap Post (f t)

users :: [User]
users = [User "john" [Post "hello", Post "world"], User "bob" [Post "foobar"]]

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Monoid m => Applicative (Const m) where
    pure  _             = Const mempty
    Const f <*> Const x = Const (f `mappend` x)

view :: Monoid a => Lens' s a -> s -> a
view ln s = getConst $ ln Const s
{-

Constの関係
 1         <$> Const "hello"      :: Num (a -> b) => Const [Char] b
 (+1)      <$> Const "hello"      :: Num b =>        Const [Char] b
 undefined <$> Const "hello"      ::                 Const [Char] b
 (:)       <$> Const "hello"      ::                 Const [Char] ([a] -> [a])
((:)       <$> Const "hello" <*>) ::                 Const [Char]  [a]         -> Const [Char] [a]

viewの定義が本物と全然違う
本物
view (traverse.posts) :: (Control.Monad.Reader.Class.MonadReader (t User) m, Traversable t) => m [Post]
      traverse.posts  :: (Traversable t, Control.Applicative.Applicative f)                 => ([Post] -> f [Post]) -> t User -> f (t User)
view                  :: Control.Monad.Reader.Class.MonadReader s m                         => Getting a s a -> m a   
独自実装 
view (traverse.posts) :: Data.Traversable.Traversable t                                     => t User -> [Post]
       traverse.posts :: (Data.Traversable.Traversable t, Applicative f)                    => ([Post] -> f [Post]) -> t User -> f (t User)
view                  :: Monoid a                                                           => Lens' s a -> s -> a

-}

main = do 
        let u = User "jack" [Post "hi"]
        print $ view posts u
        print $ view (traverse.posts) users
        print $ view (traverse.posts.traverse.title) users
        putStrLn "以下同じ結果"  
        print $ getConst $ (:) <$> Const "hello" <*> ((:) <$> Const "world" <*> pure [])
        print $ getConst $ (:) <$> Const "hello" <*> ((:) <$> Const "world" <*> Const "")
        print $ getConst $ Const $ "hello" `mappend` "world"
