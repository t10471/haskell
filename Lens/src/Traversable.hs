{-
Data.Traversableの説明

定義
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t    a  -> f (t b)
  sequenceA :: Applicative f =>               t (f a) -> f (t a)

ApplicativeはControl.Applicative

Traversableは再帰的に処理を行う

traverseの実装

instance Traversable [] where
    traverse _ [] = pure []
    traverse f (x:xs) = (:) <$> f x <*> traverse f xs

または、liftA2を使う

instance Traversable [] where
    traverse _ [] = pure []
    traverse f (x:xs) = liftA2 (:) (f x) (traverse f xs)i

liftA2のAはApplicative(liftMのMはMonad)
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
listAは以下の定義
liftA :: Applicative f => (a -> b) -> f a -> f b

traverse (\a -> if a == 0 then Nothing else  Just a) [1, 2] -> Just [1, 2]
traverse (\a -> if a == 0 then Nothing else  Just a) [1, 0] -> Nothing

sequenceAの実装

sequenceAは以下のようになるはず
sequenceA :: Applicative f => [f a] -> f [a]

instance Traversable [] where
    sequenceA [] = pure []
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
または、liftA2を使う
instance Traversable [] where
    sequenceA [] = pure []
    sequenceA (x:xs) = liftA2 (:) x (sequenceA xs)

sequenceA [Just 1, Just 2] -> Just [1, 2]
sequenceA [Just 1, Notihng] -> Nothing

sequenceAをtraverseで表現する

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA xs = traverse id xs
point free style
sequenceA    = traverse id
traverseの t (a -> f b) -> f (t b) が簡約で t (f b) -> f (t b)になる

逆にtraverseをsequenceAで定義
traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse f xs = sequenceA $ fmap f xs
point free style
traverse f    = sequenceA . fmap f

fmap :: Functor f => (a -> b) -> f a -> f b
fmapが上の定義なので、
fmap (a -> f b) t a を簡約して t (f b) になる

TraversableのfmapであるfmapDefaultの定義

型はfmapと同じ、IdentityはLensで定義してあるものと同じ Data.Functor.Identity
fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f x = runIdentity $ traverse (Identity . f) x
point free style
fmapDefault f   = runIdentity . traverse (Identity . f)

fmapDefaultはLensで定義したoverと一緒
over :: Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

TraversableのfoldであるfoldMapDefaultの定義

ConstはControl.Applicativeで定義されている
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f x = getConst $ traverse (Const . f) x
point free style
foldMapDefault f   = getConst . traverse (Const . f)

Lensで定義したviewと一緒
view :: Lens s a -> s -> a
view ln = getConst . ln Const

-}
import Data.Traversable (Traversable, fmapDefault, foldMapDefault, traverse, sequenceA)
import Control.Applicative ((<*>), pure)
import Data.Foldable (Foldable, fold, foldMap)
import Data.Monoid (Sum(..))

data List a = Nil
            | Cons a (List a)
            deriving Show

instance Functor List where
            fmap = fmapDefault

instance Foldable List where
            foldMap = foldMapDefault

instance Traversable List where
            traverse _ Nil = pure Nil
            traverse f (Cons x xs) = fmap Cons (f x) <*> traverse f xs

{-
let f x = Just (x + 1)
let xxs = (Cons 1 (Cons 2 (Cons 3 Nil)))

traverse f xxs
展開すると以下になる
traverse f (Cons x xs) = fmap Cons (f 1) <*> traverse f (Cons 2 (Cons 3 Nil))

それぞれの定義
fmap            :: Functor f => (a -> b) -> f a -> f b
Cons            :: a -> List a -> List a
(f 1)           :: Num a => Maybe a
(<*>)           :: Applicative f => f (a -> b) -> f a -> f b
traverse        :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
f               :: Num a => a -> Maybe a 
(Cons 2 (Cons 3 Nil))
                :: Num a => List a

fmap Cons       :: Functor f => f a -> f (List a -> List a)
fmap Cons (f 1) :: Num a => Maybe (List a -> List a)
(fmap Cons (f 1) <*>) 
                :: Num a => Maybe (List a) -> Maybe (List a)
簡約過程
fmap            :: Functor f => (a -> b) -> f a -> f 　　　　b
fmap Cons       :: b = (List a -> List a) に簡約
                                            f a -> f 　　　　(List a         -> List a)
fmap Cons (f 1) :: f a = Maybe a に簡約
                                      Num a => 　　　　Maybe (List a         -> List a)

(<*>)           :: Applicative f => f (a -> b) -> f a -> f b
fmap Cons (f 1) <*> ::　f = Maybe, a = List a, b = List aに簡約
                                      Num a =>             Maybe (List a) -> Maybe (List a)
traverse f (Cons 2( Cons 3 Nil)) がNum b => Maybe (List b)なので
fmap Cons (f 1) <*> traverse f (Cons 2(Cons 3 Nil)) :: Num a => Maybe (List a)
-}
main :: IO ()
main = do
          print $ traverse (\a -> if a == 0 then Nothing else  Just a) [1, 2]
          print $ traverse (\a -> if a == 0 then Nothing else  Just a) [1, 0]
          print $ sequenceA [Just 1, Just 2]
          print $ sequenceA [Just 1, Nothing]
          print $ fmapDefault (+ 1) [1,2]
          print $ getSum $ foldMapDefault Sum [1,2]
          print $ traverse (\x -> Just (x + 1)) (Cons 1 (Cons 2 (Cons 3 Nil)))
          print $ fold (Cons "hello" (Cons "world" Nil))
          print $ fmap (+1) (Cons 1 (Cons 2 (Cons 3 Nil)))
