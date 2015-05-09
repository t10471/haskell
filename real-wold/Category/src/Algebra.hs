{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
import Prelude hiding (succ,sum)

-- Haskellでは、以下のFixを使うと「函手fの最小不動点」を得る事が出来ます。
-- また、Haskellでは最小不動点と最大不動点は(細い事を気にしなければ)一致します。
-- 結局始代数・終余代数は全て以下のFixで作る事が出来ます。
--
-- 以下の定義は Fix f = f (Fix f) という不動点の等式に対応します。
newtype Fix f = In { out :: f (Fix f) }

-- F始代数の定義より
-- u . in = phi . F(u)
-- を満たすuはphiに対して一意に定まります。これにinの逆射を右から掛けると
-- u = phi . F(u) . in^(-1)
-- となるので,u = cata phiとしてそのままコードにすると以下のような汎用的な
-- 関数を得ます。

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out

-- 同様に
-- out . u = F(u) . psi
-- を満たすuがpsiに対応するanamorphismなので、
-- 以下のコードを得ます。
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi

-- ## 自然数 ##

-- NatF(X) = 1 + X
data NatF x = Zero | Succ x
    deriving (Show,Functor)

-- その不動点が自然数型。
type Nat = Fix NatF

-- 補助関数
zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)

instance Show Nat where
    show x = show (cata phi x) -- 組み込み整数に変換して表示
        where
        phi Zero = 0
        phi (Succ x) = x + 1

-- foldの例。 
-- > sqrt2 (succ (succ (succ zero)))
-- 1.4142156862745097
sqrt2 = cata phi
    where
    phi Zero = 2
    phi (Succ x) = (x+2/x)/2

-- unfoldの例。
-- > ilog2 12348712
-- 3
ilog2 = ana psi
    where
    psi x = if x `mod` 2 == 1 then Zero else Succ (x `div` 2)

-- ## リスト ##

-- ListF(X) = 1 + A*X という函手。
data ListF a x = Nil | Cons a x
    deriving (Show,Functor)

-- その不動点がリスト型。
type List a = Fix (ListF a)

-- 補助関数
nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons a as = In (Cons a as)

instance Show a => Show (List a) where
    show x = show (cata phi x)  -- 組み込みリストに変換して表示
        where
        phi Nil = []
        phi (Cons a x) = a:x

-- foldの例。
sum = cata phi
    where
    phi Nil = 0
    phi (Cons a x) = a + x

-- unfoldの例。
-- > natFrom 10
-- [10,11,12,13,14,15,....
natFrom = ana psi
    where
    psi n = Cons n (n+1)
