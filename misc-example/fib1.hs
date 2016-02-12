{-
{-# LANGUAGE NoMonomorphismRestriction #-}
-}



import Debug.Trace
noisyAdd x y = trace "adding" (x + y )


fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)
fib1 n = fibs !! n

fib2 n = fibs !! n
  where
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

fib3 = \n -> fibs !! n
  where
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)


main :: IO ()
main = do
  print $ fib1 4
  print $ fib1 5
  print $ fib2 4
  print $ fib2 5
  print $ fib3 4
  print $ fib3 5

{-
http://gelisam.blogspot.jp/2015/06/will-it-memoize.html

Will it memoize?
メモ化される？
In this post, 
この記事では下記のことを記す
I will try several variants of a simple memoizing Haskell program and test whether the program actually memoizes or not. 
いくつかの種類のメモ化されるHaskellのプログラムをためし、実際にメモ化されるかどうかテストする
This will give us a better grasp on sharing.
共有についてよりよい理解が得られるだろう

Fibonacci
フィボナッチ

If you've been studying Haskell for any length of time, 
you have probably already stumbled upon the following unusual definition of the Fibonacci sequence.
Haskellを勉強していればおそらく下記の変わったフィボナッチ数列の定義に出会っているだろう

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

If I print the fourth element of this list, how many additions will be evaluated? 
もし4つめの要素をprintするなら何回、加算が評価されているだろうか？
What if I then print the fifth element? 
5つ目をprintするならどうだろう？
Would you believe me if I told you 
that the answer depends on whether the NoMonomorphismRestriction extension is enabled or not? 
NoMonomorphismRestriction拡張の有無のよると答えたら信じるだろうか？
The answer also depends on various other details, which we will explore in this post.
他にも答えに関係している要素があるのでこれから確かめていく

NoisyAdd
うるさいAdd

In order to observe the additions as they are being evaluated, 
let's create a variant which prints a message to the console when it is called.
加算が評価されたかを観察するために、呼ばれたときにコンソールにメッセージを表示する変種を定義する

import Debug.Trace
noisyAdd x y = trace "adding" (x + y)

fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)
fib n = fibs !! n

So, how many additions when I print the fourth element?
では、4つ目をprintしたら何回加算されるだろうか？

Three, because that's the number of entries in [2..4], 
3回、なぜなら要素の中身が[2..4]だからであり、
and at each of those indices a single addition is performed to compute the next number out of the two previous numbers.
各要素一回の加算は前の2つの数字から次の数字を計算する時に実行される

How many additions when I then print the fifth?
５つ目をprintしたら何回加算されるだろうか?

Only one! There are four entries in [2..5], but entries 2 through 4 have already been evaluated during our previous query. 
たった1回 [2..5]は4つあるが、2から4はすでに前回評価されている
Implementing fib via a list of all possible results has effectively memoized our function.
fibの実装は効率的に結果をメモ化することが可能である

Inside a where clause
where句の中

The fact that fib is using a list for memoization purposes is an implementation detail. 
fibの実装の詳細はリストを使ってメモ化しているということです
Let's hide the list inside the function's where clause.
では、リストを関数のwhere句の中に隠してみましょう

fib n = fibs !! n
  where
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

Will it memoize?
メモ化されるのか？

It did not! The definitions in the where clause are local to the body of the function, 
which itself only exists once the argument n has been given. 
メモ化されなかった! where句の定義は関数の本文のローカルにあり、引数nが与えれらた時だけ存在する
For this reason, each time fib is called, 
a new copy of fibs is allocated and the previously-memoized values are forgotten.
なのでfibが呼ばれるたびに、新しいfibsのコピーが割り当てられ、以前の値は忘れられる

Consider the following variant:
下記の変種について考えてみる

fib = \n -> fibs !! n
  where
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

Will it memoize?
メモ化されるのだろうか？

It did! This time, the where block does not have access to the n argument, 
so it does not need to be instantiated on every call. 
メモ化された! 今回はwhere句が引数nに触れないので、呼び出しのたびにインスタンスを生成する必要がない
As a result, all the calls to fib are backed by the same list, and memoization succeeds.
この結果から、fibは全て背後にある同じリストから呼び出されるのでメモ化は成功する

NoMonomorphismRestriction

Let's enable the NoMonomorphismRestriction extension.
NoMonomorphismRestriction拡張を有効にしてみよう

{-# LANGUAGE NoMonomorphismRestriction #-}

fib = \n -> fibs !! n
  where
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

Adding GHC extensions allows extra non-standard features, 
GHC拡張をつけることで一般的でない特徴を許可する
but our existing standards-conforming code should work the same as before, shouldn't it? 
しかし、上記のコードは標準的なので前と同様に動くと思いませんか？
In particular, it should still memoize, right?
まだ、メモ化されると思いませんか？

It did not! Enabling NoMonomorphismRestriction changed the type which GHC inferred for our definitions. 
メモ化されなかった! NoMonomorphismRestrictionを有効にするとGHCが関数定義の推論タイプを変える
With the extension turned on, GHC infers polymorphic types:

{-# LANGUAGE ScopedTypeVariables #-}

fib :: forall a. Num a => Int -> a
fib = \n -> fibs !! n
  where
    fibs :: [a]
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

The Num a constraint is implemented via an implicit dictionary argument, 
which leads to the same non-memoizing behavior we had earlier when the n argument was in scope inside the where block. 
Num a 制約は暗黙の辞書引数として引数nがwhere句のスコープにある前述プログラムと同様にメモ化されない振る舞いとなるような実装となる
Here, Num a is clearly in scope as well, otherwise fibs would not be able to add its elements together.
Num aはスコープがはっきりしている。一方、fibsは要素を一緒に加算することができない

Without NoMonomorphismRestriction, GHC infers monomorphic types:
NoMonomorphismRestrictionがないと、GHCは単相と推論する

fib :: Int -> Integer
fib = \n -> fibs !! n
  where
    fibs :: [Integer]
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

This time the global Num Integer instance is used, not an abstract Num a which could change from call to call. 
今回は呼び出すたびに変わるかもしれない抽象的なNum aではなくIntegerインスタンスが全体で使われている
This allows the same fibs to be used on each call, which in turn allows the memoized values to be retained from call to call.
これは、それぞれ呼ばれるたびに同じfibsが使われることを許可する。 呼ばれるたびにメモ化された値を保持することが許可されているということである

Universal quantifiers
全称記号

In the type signature fib :: forall a. Num a => Int -> a, there are three arguments of different kinds: 
fib :: forall a. Num a => Int -> a の型は3つの異なる種類の引数がある
the a is an implicit type argument, the Num a is an implicit dictionary argument, and the Int is an ordinary argument. 
aは暗黙の型引数で、Num a は暗黙の辞書引数で、Intは普通の引数である
We have seen that dictionary arguments and ordinary arguments both introduce a new scope and thereby hinder memoization. 
辞書引数と普通の引数は新しいスコープを持ち込み、それによってメモ化を阻害しているようにみえる
Is it also the case for type arguments?
型引数も同様だろうか？

To figure out the answer, 
let's construct a variant of fib which always adds integers and therefore doesn't need a dictionary argument, 
and also carries around a spurious polymorphic value on which no additions are performed. 
その答えについて知るには、常にintegerを加算することにより辞書引数を不要とし、そして加算には使用しない疑似多型値を引き回すfibの変種を作成してみよう
This way, we'll have a type argument but no dictionary argument.
このようにして型引数は持っているが辞書引数を持たない関数を作成する

fib :: forall a. Int -> a -> (Integer, a)
fib = pairWithFib
  where
    pairWithFib :: Int -> a -> (Integer, a)
    pairWithFib n x = (fibs !! n, x)
    
    fibs :: [Integer]
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

Will it memoize?
メモ化されるか？

It did! Types get erased, they do not exist at runtime. 
された! 型は消されるので実行時には存在しない
In particular, instantiating a to String and later to [Double] 
does not involve passing around some runtime representation of String or [Double],
and it does not create a new copy of the where block.
特に、Stringと[Double]のインスタンス化はStringと[Double]の実行時表現を渡す必要はなく、whereブロックの新しいコピーを作成しない

It's a bit surprising, because this is a situation in which the evaluation semantics 
do not match up with the typing semantics. 
少し驚きだが、今回の評価の状況は型の文脈とは一致しない
According to the type system, 
the pairWithFib in which a has been instantiated to String is not the same pairWithFib as the one 
in which a has been instantiated to [Double], as those two pairWithFib instances have different 
types and thus cannot be interchanged. 
型システムによると、Stringをインスタンス化したpairWithFibは[Double]をインスタンス化したpairWithFibとは同じではなく、
2つのparWithFibは異なる型であり、交換は出来ない

Yet at runtime, those two instances of pairWithFib are represented by the same identical value. 
しかし、実行時、2つのpairWithFibのインスタンスは同じ同一の値を表現する
The trick is that in the absence of any type class constraint, 
pairWithFib has no way to distinguish between the different types it is supposed to be instantiated at, 
so it is allowed to behave the same in all cases. In particular, it is allowed to be the same identical value in all cases.
トリックは以下のようになっている
何の型クラス制約もないとインスタンス化した際にpairWithFibは異なる型の間に区別する方法がないので同じとして扱うことができる
全ての場合において同じ値としてみなす

Sharing
共有

In the previous paragraph, I was careful to use the phrase "same identical value" as opposed to simply "the same value". 
前段では、単に同じ値というのではなく同じ同一の値というように気をつかって使った
By "identical", I mean that the two values have the same identity, 
which in turn means that they occupy the same space in memory.
同一のによって2つの値は同じ同一性を持っていることを意味するそれは同様に同じメモリ領域を占めるという意味である

In object-oriented languages, the concept of identity is very important. 
オブジェクト指向言語において同一性の概念はとても重要である
Suppose you hold references to two objects which happen to be equal to each other, 
in the sense that their corresponding fields are pairwise equal. 
2つのオブジェクトの参照が互いに等しいということは対応しているフィールドそれぞれが等しいということだと思うだろう
Despite the fact that the objects are ostensibly equal, 
which of those two references you pass to a third party method matters because that 
third party might mutate one of the object's fields. 
2つの参照があり、第三のメソッドに渡し、第三のメソッドは片方のオブジェクトのフィールドを変更するしたとしても、表面上等しいだろう
If that happens, all the other references to this same identity, throughout your program, will also see the mutated field, 
while the references to other identities will not. 
プログラム中の同じ同一性をもつ全ての別の参照は変更がみえるだろうが、別の同一性を持った参照はそうではないだろう
And if you had passed the other reference, a different set of references, those which point to the other identity, 
will see their fields mutated. 
そして、もし別の同一性を示している別の参照、異なる参照の集合を渡したらフィールドの変更が見えるだろう
It's just normal pointer indirection, but it's something you always have to keep in mind in those languages, 
in order to avoid a family of problems known as aliasing issues.
通常のポインターの関節参照である。しかし、エイリアス問題と知られる一群の問題を避けるため、それはそれらの言語だからそうなのであるということを忘れていはいけない。


In Haskell's immutable fragment, there are no aliasing issues, 
in the sense that calling a pure function on equal values will always yield equal results, 
regardless of the identity of those values. 
Haskellの不変なフラグメントの中ではエイリアス問題はない 等しい値を使って純粋関数を呼べば常に等しい結果を返す 値の同一性は関係ない

Nevertheless, the identity of those values does have its importance: 
identities do not impact correctness, but they do impact performance. 
それでも、値の同一性は重要である 同一性は正しさの影響よりは性能への影響がある
Under some circumstances, excessive sharing can lead to poor performance, 
whereas in the case of memoization, sharing can increase performance by minimizing duplicated work.
ある状況下では、メモ化しても、過度の共有により性能が悪くなることがある
共有は小さい重複した作業において高い性能を発揮する

The general rule explaining our results in all the previous sections is 
that memoization only happens if the data structure which holds the memoized values is shared between calls. 
前章で示した結果の一般的なルールの説明は
メモ化された値は関数の呼び出しの間共有されるのでそのようなデータ構造のときだけメモ化される
ということになる

To demonstrate this, our last example plays with sharing by specifying 
which copy of the fibs data structure is used to memoize which copy of the fib function.
この最後の例のデモンストレーションは
fibsのデータ構造のコピーがfibのコピーによってメモ化される
という特定の場合の共有について試す
mkFibs :: forall a. Num a => [a]
mkFibs = fibs
  where
    fibs :: [a]
    fibs = 1 : 1 : zipWith noisyAdd fibs (tail fibs)

mkFib :: forall a. Num a => [a] -> Int -> a
mkFib = (!!)

Let's begin by constructing two copies of the fibs data structure:
2つのfibsのデータ構造をコピーした関数を作成する

fibsA :: [Integer]
fibsA = mkFibs

fibsB :: [Integer]
fibsB = mkFibs

And then two fib functions for each of the two copies of fibs:
そして、2つのfibsのコピーをもとに2つのfibを作成する

fibA1 :: Int -> Integer
fibA1 = mkFib fibsA

fibA2 :: Int -> Integer
fibA2 = mkFib fibsA

fibB1 :: Int -> Integer
fibB1 = mkFib fibsB

fibB2 :: Int -> Integer
fibB2 = mkFib fibsB

"Will it memoize" is now the wrong question to ask. 
今、メモ化さてるというの尋ねるのは悪い質問である
Each function memoizes its own results, but which functions share their memoized results with each other?
それぞれの関数は自分の結果はメモ化している、しかし、互いに関数同士は結果のメモ化を共有しているのだろうか？

> fibA1 4
adding things
adding things
adding things
5
> fibA2 5
adding things
8
> fibB1 4
adding things
adding things
adding things
5
> fibB2 5
adding things
8

As expected, functions which were created from the same copy of fibs share their results with each other.
期待通りである fibsのコピーから作られた関数は結果を互いに共有する
I hope that this last example makes it clear that sharing is something you can control. 
最後の例が共有についての理解を深めコントロールできるようになることを願いう
You can write most of your Haskell code without worrying about sharing, 
and then in the few places in which sharing affects the performance, 
you can explicitly thread your shared data structures to the places where you need them.
Haskellのコードの大部分が共有について気にする必要はなく記述でき、共有によるパフォーマンスの影響を少しの部分である
明示的に共有データ構造を必要なところで使用することができる

2015-06-20 update: as /u/pycube correctly points out, 
the above technique only works when you want to make sure that something is shared. 
Preventing sharing is much harder, because optimizations tend to increase sharing. 
With -O2, for example, all the examples in this post memoize.
-}



