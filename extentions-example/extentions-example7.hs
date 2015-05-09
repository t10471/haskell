{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

import GHC.Exts(Constraint)
import Data.Set
import Prelude hiding ((>>=),return)

-- ConstraintKinds  
-- 型シノニム(typeで定義する別名)でこの型制約を扱えるようになる
type Stringy a = (Show a, Read a)

viaString :: Stringy a => a -> a
viaString = read . show

-- 型制約をインスタンスごとに指定できるようにする

-- class instance 内にtypeを書くにはTypeFamiliesが必要
class RMonad m where
  type RMonadCxt m a :: Constraint -- 型制約であることの明示
  type RMonadCxt m a = ()          -- デフォルトの型制約
  return :: (RMonadCxt m a) => a -> m a
  (>>=)  :: (RMonadCxt m a, RMonadCxt m b) => m a -> (a -> m b) -> m b

instance RMonad [] where
  return x = [x]
  (>>=)    = flip concatMap

instance RMonad Set where
  type RMonadCxt Set a = Ord a -- Set 独自の型制約
  return  = singleton
  s >>= f = fromList [e' | e <- toList s, e' <- toList (f e)]

main :: IO ()
main = do
   let a = fromList [0..100] >>= \x -> singleton (x*2)
   print a

