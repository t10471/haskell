{-# LANGUAGE RankNTypes #-}

{- Lensのkindは (* -> * -> *)
   forall (f :: * -> *). Functor f => (a -> f a) -> s -> f s
   これのためにRankNTypesが必要
-}
type Lens s a = Functor f => (a -> f a) -> s -> f s

{- newtypeはtypeとdataの中間みたいなもん
   typeは単に別名にするだけ。
   newtypeも別名をつけるみたいなものだが、1つのレコードしかもてない。
-}
newtype Identity a = Identity { runIdentity :: a }
-- Funcotorは(* -> *)のKindしかとれない
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

{- let ln = undefined :: (Functor f => (a -> f a) -> (s -> f s))
   let f = undefined :: (a -> a)
   let s = undefined :: s

   Identity :: a -> Identity a
   ln Identity :: s -> Identity s
   Identity . f :: b -> Identity b
   ln (Identity . f) :: s -> Identity s
   runIdentity $ ln (Identity . f) s :: a
   over ageLens :: (Int -> Int) -> User -> User
   let john = User { name = "John", age = 30 }
   
   overは結局こうなる。+1をIdentityでラップする
   runIdentity $ fmap (\newAge -> john { age = newAge }) ((Identity . (+1)) (age john))
-}

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ ln (Identity . f) s

--over :: Lens s a -> (a -> a) -> s -> s
--over ln f = runIdentity . ln (Identity . f)

{- Const :: * -> * -> *
   Const Int :: * -> *
   instance宣言はkind、fmapは値コンストラクタ
   Const 1 2はエラー Constは1引数しか取れない
   
   let cJohn = Const (name john)
   cJohn :: Const String b 
   fmap (+1) cJohn :: Num b => Const String b

   fmap :: Functor f => (a -> b) -> f a -> f b
   fmap (+1) :: (Num b, Functor f) => f b -> f b
   fmap (+1) (Const "a") :: Num b => Const [Char] b
   bをつかってfmapを無効化している
-}
newtype Const a b = Const { getConst :: a }
instance Functor (Const a) where
  fmap _ (Const a) = Const a

{- fmap (&& False) (Const "hello") :: Const [Char] Bool
   ln Const :: s -> Const a s
   ln Const s :: Const a s
   結局こうゆうこと
   getConst $ (fmap (\newName -> john { name = newName }) (Const (name john)))
-}
view :: Lens s a -> s -> a
view ln s = getConst $ ln Const s

{- const :: a -> b -> a
   第一引数をそのまま返す
   結局こうなる
   runIdentity $ fmap (\newAge -> john { age = newAge }) ((Identity .  (const 21 )) (age john))
-}
set :: Lens s a -> a -> s -> s
set ln x = over ln (const x)

_1 :: Functor f => (a -> f a) -> (a,b) -> f (a,b)
_1 f (x,y) = fmap (\a -> (a, y)) (f x)

{- view _1 (1,2)      -> 1
   set  _1 3 (1,2)    -> (3,2)
   over _1 (+3) (1,2) -> (4, 2)
-}
data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

nameLens :: Lens User String
nameLens f user = fmap (\newName -> user { name = newName }) (f (name user))

ageLens :: Lens User Int
ageLens f user = fmap (\newAge -> user { age = newAge }) (f (age user))

ownerLens :: Lens Project User
ownerLens f project = fmap (\newOwner -> project { owner = newOwner }) (f (owner project))

ownerNameLens :: Lens Project String
ownerNameLens = ownerLens.nameLens

main = do let john = User { name = "John", age = 30 }
          let p = Project { owner = john }
          print $ view ownerNameLens p
          print $ set ownerNameLens "Bob" p
          print $ over ageLens (+1) john

