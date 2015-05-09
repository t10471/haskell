{-# LANGUAGE TypeFamilies #-}

type family Ident a :: *
type instance Ident Int  = Int
type instance Ident Char = Int

ident :: Ident Int -> Ident Int
ident x = x + 1

ident' :: Ident Char -> Ident Char
ident' x = x + 1

main :: IO ()
main = do
  print $ ident  3
  print $ ident' 3
