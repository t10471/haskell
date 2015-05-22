{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

-- 最初の値が Bool 型である非空のリストの頭部を取る
headBool :: HList (Bool ': xs) -> Bool
headBool hlist = case hlist of
  (a ::: _) -> a

hlength :: HList x -> Int
hlength Nil = 0
hlength (_ ::: b) = 1 + (hlength b)


tuple :: (Bool, (String, (Double, ())))
tuple = (True, ("foo", (3.14, ())))

hlist :: HList '[Bool, String , Double , ()]
hlist = True ::: "foo" ::: 3.14 ::: () ::: Nil
