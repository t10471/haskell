{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE EmptyDataDecls #-}

{-
1.各ノードは赤か黒の色をもつ。
2.根は黒である。
3.葉はすべて黒である（したがって、赤のノードは子をもつ）。
4.赤のノードの子ノードはすべて黒である（したがって、対偶より、赤のノードの親ノードは黒である）。
5.任意のノードについて、そのノードから子孫の葉までの道に含まれる黒いノードの数は、
  選んだ葉によらず一定である
  （この条件は、「根から葉までの道に含まれる黒いノードの数は、葉によらず一定である」と言い換えることができる）。
-}
data Black -- This is what EmptyDataDecls allows,
data Red   -- types with no constructors

data Tree :: * -> * -> * where
  Leaf  :: Tree a Black
  -- ルート
  NodeR :: a -> Tree a Black -> Tree a Black -> Tree a Red
  -- 枝
  NodeB :: a -> Tree a c     -> Tree a c'    -> Tree a Black

-- 間違った型でもエラーにならない
crazy :: Tree a Int
crazy = undefined

