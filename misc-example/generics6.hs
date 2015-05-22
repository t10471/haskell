{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Generics.Uniplate.Direct
{-
Biplate では、ターゲットの型が元の型と同じである必要のないプレートを一般化していて、
サブターゲットの型を示すために多引数型クラスを使っています。
Uniplate の関数はすべて Biplate においても、対応する一般化された形の関数を持ちます。

descendBi    :: Biplate from to            => (to ->   to) -> from -> from
transformBi  :: Biplate from to            => (to ->   to) -> from -> from
rewriteBi    :: Biplate from to            => (to -> Maybe to)     -> from -> from

descendBiM   :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
transformBiM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
rewriteBiM   :: (Monad m, Biplate from to) => (to -> m (Maybe to)) -> from -> m from
-}


type Name = String

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  deriving Show

data Stmt
  = Decl [Stmt]
  | Let Name Expr
  deriving Show

instance Uniplate Expr where
  uniplate (Var x  ) = plate Var |- x
  uniplate (App x y) = plate App |* x |* y
  uniplate (Lam x y) = plate Lam |- x |* y

instance Biplate Expr Expr where
  biplate = plateSelf

instance Uniplate Stmt where
  uniplate (Decl x  ) = plate Decl ||* x
  uniplate (Let x y)  = plate Let |-  x |- y

instance Biplate Stmt Stmt where
  biplate = plateSelf

instance Biplate Stmt Expr where
  biplate (Decl x)  = plate Decl ||+ x
  biplate (Let x y) = plate Let |- x |* y

rename :: Name -> Name -> Expr -> Expr
rename from to = rewrite f
  where
    f (Var a)   | a == from = Just (Var to)
    f (Lam a b) | a == from = Just (Lam to b)
    f _                     = Nothing

s, k, sk :: Expr
s  = Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
k  = Lam "x" (Lam "y" (Var "x"))
sk = App s k

m :: Stmt
m = descendBi f $ Decl [ (Let "s" s) , Let "k" k , Let "sk" sk ]
  where
    f = rename "x" "a"
      . rename "y" "b"
      . rename "z" "c"

main :: IO ()
main = do
  putStrLn "end"
