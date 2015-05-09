{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

import Prelude hiding (Functor, fmap, Either(..))
import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Data.Function (on)
import Control.Monad.Trans.Class(MonadTrans(..))
import qualified Data.Map as Map

{-
          g   ::   a    ->   b
fmap      g   :: f a    -> f b

          g   ::   a    <-   b
contramap g   :: f a    -> f b

          g   ::   a    ->   c
            h ::     b  ->     d
bimap     g h :: f a b  -> f c d  

          g   ::   a    <-   c
            h ::     b  ->     d
dimap     g h :: f a b  <- f c d

-}

data  Either a b  =  Left a | Right b
  deriving (Show)

type StrInt = Either String Int

class Functor f where
  fmap :: (a->b) -> f a -> f b

instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right y) = Right (f y)


-- 双ファンクタ
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g
  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id
  second :: (b -> c) -> p a b -> p a c
  second = bimap id

instance Bifunctor Either where
  bimap f _ (Left a)  = Left (f a)
  bimap _ g (Right b) = Right (g b)

bil :: StrInt
bil = Left "str"
bir :: StrInt
bir = Right 1

exB = do
  print $ bimap (++ " bimap") (+ 10) bil
  print $ bimap (++ " bimap") (+ 10) bir

-- 反変ファンクタ
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  --                             (b -> Bool) . (a -> b)
  contramap f g = Predicate $ getPredicate g . f

veryOdd :: Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

exC = print $ getPredicate veryOdd <$> [0 .. 11]

-- 双対関数反変ファンクタ
newtype Op a b = Op {getOp :: b -> a}
instance Contravariant (Op a) where
  contramap f g = Op $ getOp g . f

newtype Const a b = Const a deriving Show
instance Contravariant (Const a) where
  contramap _ (Const a) = Const a

newtype Comparison a = Comparison {getComparison :: a -> a -> Ordering} -- e.g. compare
instance Contravariant Comparison where
  contramap g (Comparison comp) = Comparison (comp `on` g)

exC2 =  comp 1 2
  where comp = getComparison $ contramap negate $ Comparison compare

-- プロ関手
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id

-- 以下Profunctorの実例がだよくわからない

type Limits a = Limits' a a
data Limits' a b = Limits { step :: a -> (b, b)
                          , check :: a -> a -> Bool } 

instance Profunctor Limits' where
    dimap g h Limits {..} = Limits  { step = (h *** h) . step . g
                                    , check = check `on` g }

maybeLimit :: a -> Limits a -> Limits (Maybe a)
maybeLimit d = dimap (maybe d id) Just

millionsLimit :: Limits Double -> Limits Double
millionsLimit = dimap (1.0e6 *) (/ 1.0e6)

sm a = (a,a)
ss a b = a == b
exP = maybeLimit 1 $ Limits {step = sm, check = ss}
exD = millionsLimit $ Limits {step = sm, check = ss}


newtype Indexed i a b = Indexed { runIndexed :: i -> a -> b }

instance Profunctor (->) where 
  dimap g h f = h . f . g

instance Profunctor (Indexed i) where
  dimap g h (Indexed f) = Indexed (dimap g h . f)

class Indexable i p where
  indexed :: p a b -> i -> a -> b

instance Indexable i (Indexed i) where indexed = runIndexed
instance Indexable i (->)        where indexed = const

mapIndexable :: Indexable i p => p a b -> Map.Map i a -> Map.Map i b
mapIndexable = Map.mapWithKey . indexed

idx :: Int -> String -> String
idx i a = case i `mod` 2 of
            1 -> a ++ " is odd"
            0 -> a ++ " is even"

mkIdx = Indexed idx

mkMap :: Map.Map Int String
mkMap = Map.fromList [(1, "one"), (2, "two")]

-- モナドを変換するファンクタとモナド
class MFunctor t where
  hoist :: (Monad m) => (forall a . m a -> n a) -> t m b -> t n b
class (MFunctor t, MonadTrans t) => MMonad t where
  embed :: (Monad n) => (forall a . m a -> t n a) -> t m b -> t n b

-- 米田の補題
newtype Yoneda f a = Yoneda { runYoneda :: forall b. ((a -> b) -> f b) } 
instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))
  
data CoYoneda f a = forall b. CoYoneda (b -> a) (f b)
instance Functor (CoYoneda f) where
  fmap f (CoYoneda g v) = CoYoneda (f . g) v


main :: IO ()
main = do
  print $ Map.map (++ " add") mkMap
  print $ Map.mapWithKey idx  mkMap
  print $ mapIndexable mkIdx mkMap
  putStrLn "end"


