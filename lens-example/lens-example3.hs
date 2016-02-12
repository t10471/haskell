{-# LANGUAGE TemplateHaskell#-}

import Control.Lens
import Data.Text.Lens
import Data.Char
import Data.Data.Lens
import Data.Functor


data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a  }
makeLenses ''Foo

{-
以下が自動で作成される
bar, baz :: Simple Lens (Foo a) Int
quux :: Lens (Foo a) (Foo b) a b


色々な定義
type Lens      s t a b = forall f. Functor f => 
-                        (a -> f b)          -> s -> f t

type Getter    s   a   = forall f. (Contravariant f, Functor f) => 
-                        (a -> f a)          -> s -> f s
type Getting r s   a   = (a -> Const r a)    -> s -> Const r s

type Setter    s t a b = forall f. Settable f => 
-                        (a -> f b)          -> s -> f t
type Setting p s t a b = p a (Identity b)    -> s -> Identity t

type Iso       s t a b = forall p f. (Profunctor p, Functor f) => 
-                        p a (f b)           -> p s (f t)

type Prism     s t a b = forall p f. (Choice p, Applicative f) => 
-                        p a (f b)           -> p s (f t)

type Equality  s t a b = forall p f. 
-                        p a (f b)           -> p s (f t)

type Review      t   b = forall p f. (Choice p, Bifunctor p, Settable f) => 
-                        Optic' p f t b

type Fold      s   a   = forall f. (Contravariant f, Applicative f) => 
-                        (a -> f a ) -> s -> f s
type Traversal s t a b = forall f. Applicative f => 
-                        (a -> f b ) -> s -> f t


Lens

lens ::              (s -> a ) -> (s -> b -> t ) -> Lens s t a b
下記のように分解出来る
lens :: Functor f => (s -> a ) -> (s -> b -> t ) -> (a -> f b ) -> s -> f t

実装
lens sa sbt afb s = sbt s <$> afb (sa s)

>>> s ^. lens getter setter
getter s
>>> s & lens getter setter .~ b
setter s b
>>> s & lens getter setter %~ f
setter s (f (getter s) )
lens :: (s -> a ) -> (s -> a -> s ) -> Lens' s a


Getter Setter

AccessorはConst MutatorはIdentity

view :: MonadReader s m => Getting a s t a b -> m a
view l = Reader.asks (runAccessor# (l Accessor))

set :: Setting s t a b -> b -> s -> t
set l b = runMutator# (l (\_ -> Mutator b))

(^.) :: s -> Getting a s t a b -> a
s ^. l = getConst (l Const s)

(.~) :: Setting s t a b -> b -> s -> t
l .~ b = runIdentity (l (\_ -> Identity b))

-- newtype Const a b = Const { getConst :: a }
-- newtype Identity a = Identity { runIdentity :: a }

Iso 
同型を表す

iso :: Functor f => (s -> a) -> (b -> t) -> Iso s t a b 
from :: AnIso s t a b -> Iso b a t s

isoの実装
iso sa bt = dimap sa (fmap bt)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
Profunctor
dimap :: (c -> a) -> (b -> d) -> f a b -> f c d
-     g   ::   a   <-   c
-       h ::     b ->     d
dimap g h :: f a b -> f c d

isoは以下のような性質を満たします
view       (iso f g)    ≡ f
view (from (iso f g))   ≡ g
over       (iso f g)  h ≡ g . h . f
over (from (iso f g)) h ≡ f . h . g

isoの図
b      |   [a    ->    f b]
-      |
↓ bt　|   ↑sa　     ↓fmap bt
-      |
t      |   [s    ->    f t]

LensはIsoを->に特殊化したもの

newtype Neither a b = Neither { _nor :: Either a b  } deriving (Show)
makeIso ''Neither

下記が作成される

neither :: Iso (Neither a b) (Neither c d) (Either a b)  (Either c d)
nor     :: Iso (Either a b)  (Either c d)  (Neither a b) (Neither c d)

以下を満たす
from neither = nor
from nor = neither
neither.nor = id
nor.neither = id


Prism 
Isoの特殊化

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b

nat :: Simple Prism Integer Natural
nat = prism toInteger $ \ i ->
    if i < 0
    then Left i
    else Right (fromInteger i)

>>> (-3,4) & both.nat *~ 2

>>> (3 :: Integer ) ^? nat :: Maybe Natural
Just 3
>>> (-3 :: Integer) ^? nat :: Maybe Natural
Nothing
>>> (3 :: Natural ) ^. re nat :: Integer
3
>>> (3 :: Integer ) ^?! nat :: Maybe Natural
3
>>> (-3 :: Integer) ^? nat :: Maybe Natural
*** Exception: (^?!): empty Fold
>>> (3 :: Natural ) ^? re nat :: Maybe Integer
Just 3
>>> (-3 :: Integer) & nat %~ (* 2 )
-3
>>> (3 :: Integer ) & nat %~ (* 2 )
6

data ABC = A | B Int | C Double String
makePrisms ''ABC

下記の定義が作成される

_A :: Prism' ABC ()
_B :: Prism' ABC Int
_C :: Prism' ABC (Double, String)

Equality
(a ~ s, b ~ t)を表す

data Identical a b s t where
  Identical :: Identical a b a b


Review
reするとGetterになるPrism

unto :: (Profunctor p, Bifunctor p, Functor f) => (b -> t ) -> Optic p f s t a b
un :: (Profunctor p, Bifunctor p, Functor f) => Getting a s a -> Optic' p f a s
re :: Contravariant f => AReview t b -> LensLike' f b t
review :: MonadReader b m => AReview t b -> m t
reuse :: MonadState b m => AReview t b -> m t

review  ≡ view  . re
reviews ≡ views . re
reuse   ≡ use   . re
reuses  ≡ uses  . re
review . unto ≡ id
reuse ≡ use . re
reuse . unto ≡ gets

re :: Prism s t a b -> Getter b t
re :: Iso s t a b   -> Getter b t

review :: Iso' s a   -> a -> s
review :: Prism' s a -> a -> s

review :: MonadReader a m => Iso' s a   -> m s
review :: MonadReader a m => Prism' s a -> m s

reuse :: MonadState a m => Prism' s a -> m s
reuse :: MonadState a m => Iso' s a   -> m s

>>> 5 ^.re _Left
Left 5
>>> 6 ^.re (_Left.unto succ)
Left 7

>>> review _Left "mustard"
Left "mustard"
>>> review (unto succ) 5
6

>>> evalState (reuse _Left) 5
Left 5
>>> evalState (reuse (unto succ)) 5
6

Fold

type Fold s a = forall m. Monoid m => Getting m s a

folded    :: Foldable   f => IndexedFold Int (f a ) a
folding   :: Foldable   f => (s -> f a ) -> Fold s a
foldMapOf :: Profunctor p => Accessing p r s a -> p a r -> s -> r
pre       :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
preview   :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preuse    :: MonadState  s m => Getting (First a) s a -> m (Maybe a)



Traversal

type Traversal s t a b = forall f. Applicative f => (a -> f b ) -> s -> f t


traverseOf  :: Over p f s t a b -> p a (f b ) -> s          -> f t
forOf       :: Over p f s t a b -> s          -> p a (f b ) -> f t
sequenceAOf :: LensLike f s t (f b ) b        -> s          -> f t
mapMOf      :: Profunctor p => Over p (WrappedMonad m) s t a b -> p a (m b ) -> s -> m t

traverse  :: Applicative f   => (a -> f b ) -> t a -> f (t b )
both      :: Bitraversable r => Traversal (r a a ) (r b b ) a b
traversed :: Traversable f   => IndexedTraversal Int (f a ) (f b ) a b

-}

ex = do
  print $ fmap succ [1,2,3]
  -- fmap == over mappend
  print $ over mapped succ [1,2,3]
  print $ over (mapped._2) succ [(1,2),(3,4)]
  -- overの中置演算子
  print $ _1.mapped._2.mapped %~ succ $ ([(42, "hello")],"world")
  -- コンテナ系はfolded
  print $ allOf (folded.text) isLower ["hello", "goodbye"]
  -- ジェネリクスのuniplate系も使える
  print $ anyOf biplate (=="world") ("hello",(),[(2::Int,"world")])
  -- traverse系
  mapMOf (traverse._2) (\xs -> length xs <$ putStrLn xs) [(42,"hello"),(56,"world")]
