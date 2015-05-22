
import Data.Generics.Uniplate.Direct

{-
データ構造を走査と変換ができる
plate :: from -> Type from to
(|*)  :: Type (to -> from) to -> to -> Type from to
(|-)  :: Type (item -> from) to -> item -> Type from to

descend   :: Uniplate on => (on -> on) -> on -> on
transform :: Uniplate on => (on -> on) -> on -> on
rewrite   :: Uniplate on => (on -> Maybe on) -> on -> on
descend   関数は式の直下の各子孫に関数を適用して親の式に結果を集めてきます。
transform 関数は式の全ての項をボトムアップで変換する一つの道筋を進みます。
rewrite   関数は式の全ての項を不動点まで完全に変換し尽くします。Maybe は停止を表しています。
-}


data Expr a
  = Fls
  | Tru
  | Var a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or  (Expr a) (Expr a)
  deriving (Show, Eq)

instance Uniplate (Expr a) where
  uniplate (Not f)     = plate Not |* f
  uniplate (And f1 f2) = plate And |* f1 |* f2
  uniplate (Or  f1 f2) = plate Or  |* f1 |* f2
  uniplate x           = plate x

simplify :: Expr a -> Expr a
simplify = transform simp
 where
   simp (Not (Not f)) = f
   simp (Not Fls)     = Tru
   simp (Not Tru)     = Fls
   simp x             = x

reduce :: Show a => Expr a -> Expr a
reduce = rewrite cnf
  where
    -- 二重否定
    cnf (Not (Not p))        = Just p

    -- ドモルガン
    cnf (Not (p `Or` q))     = Just $ (Not p) `And` (Not q)
    cnf (Not (p `And` q))    = Just $ (Not p) `Or` (Not q)

    -- 論理積の分配則
    cnf (p `Or` (q `And` r)) = Just $ (p `Or` q) `And` (p `Or` r)
    cnf ((p `And` q) `Or` r) = Just $ (p `Or` q) `And` (p `Or` r)
    cnf _                    = Nothing


example1 :: Expr String
example1 = simplify (Not (Not (Not (Not (Var "a")))))
-- Var "a"

example2 :: [String]
example2 = [a | Var a <- universe ex]
  where
    ex = Or (And (Var "a") (Var "b")) (Not (And (Var "c") (Var "d")))
-- ["a","b","c","d"]

example3 :: Expr String
example3 = reduce $ ((a `And` b) `Or` (c `And` d)) `Or` e
  where
    a = Var "a"
    b = Var "b"
    c = Var "c"
    d = Var "d"
    e = Var "e"

{-
以下でも出来るがオーバーヘッドが自分で記述したものよりかかる
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data

data Expr a
  = Fls
  | Tru
  | Lit a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or (Expr a) (Expr a)
  deriving (Data, Typeable, Show, Eq)
-}

main :: IO ()
main = do
  putStrLn "end"
