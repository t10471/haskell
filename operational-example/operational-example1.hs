{-# LANGUAGE GADTs #-}


import Control.Monad.Operational


type Stack = [Int]

data StackI a where
    Push :: Int -> StackI ()
    Pop :: StackI Int

type StackP a = Program StackI a


push :: Int -> StackP ()
push = singleton . Push

pop :: StackP Int
pop = singleton Pop

{-
関数viewは、命令ツリーを分解する
先頭の命令を表す型StackIのデータ
残りの命令ツリーを表す型(b -> Program Stack I a )のデータ

Pushの場合、与えられた値をStackの先頭に追加して、残りの命令ツリーを処理するように、インタプリタ関数を再帰呼び出し
Pushは戻り値を持たないように定義しているので、残りの命令ツリーに対して、ユニット()を渡す

Popの場合、Stackの先頭からデータを取り出し、残りの命令ツリーを処理するように、インタプリタ関数を再帰呼び出し
PopはStackの先頭データが戻り値なので、残りの命令ツリーに対して、その値を渡す

Returnは、命令ツリーの末尾に達したことを表す、Operationalモナドで予め定義された命令
今回は、Stackの最終結果を返すので、渡されたStackをそのまま返しています。命令ツリーの末尾なので、再帰呼び出しは行われません。
-}
interpret :: StackP a -> Stack -> Stack
interpret = eval . view
  where
    eval :: ProgramView StackI a -> Stack -> Stack
    eval (Push x :>>= is) stack     = interpret (is ()) (x:stack)
    eval (Pop    :>>= is) (x:stack) = interpret (is x) stack
    eval (Return _) stack           = stack


add :: StackP ()
add = do
    x <- pop
    y <- pop
    push (x + y)


main :: IO ()
main = (putStrLn . show) $ interpret add [1, 2]

