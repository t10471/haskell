{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE StandaloneDeriving   #-}

import GHC.Exts(Constraint)

data SList (as :: [*]) where
    SNil  ::                  SList '[]
    (:::) :: a -> SList xs -> SList (a ': xs)
infixr :::

type family Fn (as :: [*]) r
type instance Fn '[] r = r
type instance Fn (x ': xs) r = x -> Fn xs r

apply :: Fn xs r -> SList xs -> r
apply v SNil = v
apply f (a ::: as) = apply (f a) as

type family Snoc (as :: [*]) a :: [*]
type instance Snoc '[] a = a ': '[]
type instance Snoc (x ': xs) a = x ': Snoc xs a

sSnoc :: SList as -> a -> SList (Snoc as a)
sSnoc SNil a       = a ::: SNil
sSnoc (x ::: xs) a = x ::: sSnoc xs a

type family All (c :: * -> Constraint) (as :: [*]) :: Constraint
type instance All c '[] = ()
type instance All c (a ': as) = (c a, All c as)

deriving instance All Show as => Show (SList as)

main :: IO ()
main = do
  print $ apply (+) (3 ::: SNil) (8::Int)
  print $ sSnoc (13 ::: "t" ::: SNil) 4.1
  print $ ("test"::String) ::: (3::Int) ::: SNil

