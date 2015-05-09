import Data.Function

-- 実際に動くコードで最小不動点のアイデアを理解しましょう。
-- fact(n) = n!の例を使います。

-- 以下のfactFの最小不動点がfactです。
factF :: (Integer -> Integer) -> (Integer -> Integer)
factF f = \n -> if n == 0 then 1 else n*f(n-1)

-- すると以下のような関数の列が出来ます。
-- factNはN未満の自然数nについては正しくn!を返し,N以上の自然数についてはundefinedを返します。
fact0 = undefined
fact1 = factF undefined
fact2 = factF (factF undefined)
fact3 = factF (factF (factF undefined))
fact4 = factF (factF (factF (factF undefined)))
fact5 = factF (factF (factF (factF (factF undefined))))

-- 求める関数factはこの列の極限です。
-- Haskellの標準ライブラリには最小不動点を求める為の関数fixが用意されており
-- 以下の様にfactを得る事が出来ます。
fact = fix factF
