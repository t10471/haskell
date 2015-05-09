{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Control.Monad.Identity

-- Const の使い方
-- Lens での使用例

-- The definition of Van Laarhoven lenses:
type Lens a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- Getter passes the Const functor to the lens:
get :: Lens a b -> a -> b
get l = getConst . (l Const)
{-
Const に対して fmap をすると第一引数の関数が無視されて
第二引数の値を返す
getConst $ fmap (\x -> x + 1) $ Const 1
=> 1
-}

-- Updater passes the Identity functor to the lens:
modify :: Lens a b -> (b -> b) -> (a -> a)
modify l f = runIdentity . l (Identity . f)

set :: Lens a b -> b -> (a -> a)
set l r = modify l (const r)

-- Example: -------------------------------------------

data Person = Person { _name :: String, _age :: Int }
  deriving Show

name :: Lens Person String
name f (Person n a) = fmap (\x -> Person x a) (f n)

age :: Lens Person Int
age f (Person n a) = fmap (\x -> Person n x) (f a)

main :: IO ()
main = do
    let john = Person "John" 34
    print $ get age john
    print $ set name "Pete" john
