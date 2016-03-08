{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}

import Prelude hiding (log)
import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

{-
 This text deals with a way to compose certain kinds of monads, thereby mixing their capabilities. 
 この文章はある種類のモナドを、その能力を混ぜ合わせて合成する方法について扱う. 
 It is a literate Haskell file, so let's begin with a bunch of noise.
 全く訳が分からない

A motivating problem
問題の動機

Here are two functors and their free monads which define DSLs for addition of integers, and for logging.
ここに2つのファンクターとフリーモナドによる加算とログ出力のDSLがある
-}

data PlusF a where
   Plus :: Int -> Int -> (Int -> a) -> PlusF a

instance Functor PlusF where
    fmap f (Plus left right next) = Plus left right (fmap f next)

data LogF s a where
    Log :: s -> a -> LogF s a

instance Functor (LogF s) where
    fmap f (Log s next) = Log s (f next)

type Plus  = Free PlusF
type Log s = Free (LogF s)

plus :: Int -> Int -> Plus Int
plus i j = liftF $ Plus i j id

log :: s -> Log s ()
log s = liftF $ Log s ()

{-
Either of these free monads can be interpreted using iterM and an appropriate interpreter function.
どちらのフリーモナドもiterMと適した解釈関数を使って解釈することができる
-}

type Interpreter f m = forall a . f (m a) -> m a
type TypeOfIterM     = forall f m a . (Functor f, Monad m) => Interpreter f m -> Free f a -> m a

{-
A Log String term, for example, can be interpreted by IO:
Log String は IO によって解釈される
-}

stdoutLog' :: Interpreter (LogF String) IO
stdoutLog' term = case term of
    Log str next -> putStrLn str >> next

runStdoutLog :: Log String t -> IO t
runStdoutLog = iterM stdoutLog'

{-
Similarly, we could interpet Plus using a reader which holds a modulus like so:
同様にPlusはモジュロを保持したReaderを使って下記のように解釈出来る
-}

modularPlus' :: Interpreter PlusF (Reader Int)
modularPlus' term = case term of
    Plus left right next -> do
        modulus <- ask
        next ((left + right) `mod` modulus)

runModularPlus :: Plus t -> Reader Int t
runModularPlus = iterM modularPlus'

{-
But suppose we want to mix Plus and Log String, so that we can add things and also log strings. 
しかし、PlusとLog Stringを合成して、加算してログ出力していとおもうと、
We have interpreters for each of these; it would be nice to use them to define an interpreter for the composite. 
それぞれのインタープリターが必要になる. 合成したインタープリターを定義し使うことができればいいのだが.
This text shows one way to achieve this.
下記にそれを成し遂げる一つの方法を示す

A solution
解決

To begin, we need the tools to define the monad composite of Plus and Log. 
最初に、PlusとLogを合成したモナドを定義するためのツールが必要である
The functor sum allows us to build new functors from old, 
and free monads on these sums are monads which contain the terms of each summand functor, which is just what we need.
合成ファンクターは古いファンクターから新しいファンクターを合成することができる。
そして、合成フリーモナドはそれぞれのファンクターの合成したものを含んでいる。
これらは欲しかったものである
-}

infixr 8 :+:
data (f :+: g) a = SumL (f a) | SumR (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f term = case term of
        SumL x -> SumL (fmap f x)
        SumR x -> SumR (fmap f x)

{-
For any functor f, we can always inject it into a free monad over a functor sum in which it appears as a summand.
どんなファンクターfでも いつもフリーモナドに合成されたように見えるファンクターを注入することができる

These are easy to spot:
見つけつのは簡単
-}

-- injFL :: Functor f => Free f a -> Free (f :+: g) a
-- injFL term = case term of
--     Pure x -> Pure x
--     Free x -> Free (SumL (fmap injFL x))
--
-- injFR :: Functor f => Free f a -> Free (g :+: f) a
-- injFR term = case term of
--     Pure x -> Pure x
--     Free x -> Free (SumR (fmap injFR x))
--
-- 上の一般化

{-
Using typeclasses, we can extend this to sums of more than two summands, just like a construction from datatypes à la carte.
型クラスを使い、データタイプアラカルトのように2つの合成を拡張することができる
-}

class InjectSum f g where
    inject :: f a -> g a

instance InjectSum f f where
    inject = id

instance InjectSum f (f :+: h) where
    inject = SumL

instance (InjectSum f h) => InjectSum f (g :+: h) where
    inject = SumR . inject

injectF_ :: (Functor g, InjectSum f g) => f a -> Free g a
injectF_ = liftF . inject

injectF :: (Functor f, Functor g, InjectSum f g) => Free f a -> Free g a
injectF term = case term of
    Pure a     -> Pure a
    Free fterm -> Free (inject (fmap injectF fterm))

{-
Now we can tell GHC of our composite monad, LogPlus, and write terms in it.
これで、GHCに合成モナドLogPlusを理解させ、使うことができる
-}

type LogPlusF = LogF String :+: PlusF
type LogPlus  = Free LogPlusF

logPlusTerm :: Int -> LogPlus Int
logPlusTerm i = do
    injectF $ log ("Input is " ++ (show i))
    sum <- injectF $ plus i 42
    injectF $ log ("Sum computed successfully!")
    return sum

{-
But what use is this, when we have no way to interpret a LogPlus? 
しかし、LogPlusを解釈する方法がない場合はどうやってつかえばいいのか？
We need something of type LogPlusF (m a ) -> m a, and we need to somehow produce it from the existing interpreters for Log and Plus.
LogPlusF (m a ) -> m aという型の何かが必要で、存在するLogやPlusのインタープリターから、それどうにかして生成する必要がある
If we could do this, then what would m be in the type above? 
これをするには、上のmの型は何でしょうか？
It must in some sense contain the smaller interpreters, IO for Log and Reader Int for Plus.
小さいインタープリターが含まれるはずである。Logに対するIOとPlusに対するReader Int
I can think of one such monad, namely:
文字通り下記のようなモナドが考えられる
-}

type CompositeInterpreter = IdentityT (ReaderT Int IO) 

{-
And this begs us to think that maybe, if both functors in a sum have interpreters which are monad transformers, 
合成されたインタープリターの両方のファンクターがモナドトランスフォーマーだと考えてほしい
then the interpreter for their sum is these two transformers stacked atop one-another.
そして、合成されたインタープリターは積み重ねられたトランスフォーマーである
The type :&: is used to denote this stacking, so that (IdentityT :&: ReaderT Int) IO = CompositeInterpreter.
:&:の型は積み重ねられたものを表しているので (IdentityT :&: ReaderT Int) IO = CompositeInterpreter である
-}

infixr 8 :&:
newtype ((m :: (* -> *) -> * -> *) :&: (n :: (* -> *) -> * -> *)) (s :: * -> *) t = Trans {
    runTrans :: m (n s) t
  } deriving (Functor, Applicative, Monad, MonadIO)

{-
If we rephrase the interpreters for Log and Plus as transformers, then we can write the type of the Interpreter which can handle their sum.
LogとPlusのインタープリターをトランスフォーマーと言い換える。それは合成されたインタープリターの型として記述出来る
-}


newtype ModularPlusInterpreter m t = MPI {
    runMPI :: ReaderT Int m t
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

newtype StdoutLogInterpreter m t = SLI {
   runSLI :: IdentityT m t
 } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


-- Interpreterにフリーモナドとインタープリターを持たせることで解釈する関数を作成

-- type Interpreter f m = forall a . f (m a ) -> m a
modularPlus :: Monad m => Interpreter (PlusF) (ModularPlusInterpreter m)
--          term = PlusF (ModularPlusInterpreter m a)
modularPlus term = case term of
    Plus left right next -> do
        modulus <- MPI ask
        next ((left + right) `mod` modulus)

stdoutLog :: MonadIO m => Interpreter (LogF String) (StdoutLogInterpreter m)
--        term = LogF String (StdoutLogInterpreter m a)
stdoutLog term = case term of
    Log str next -> do
        SLI (liftIO (putStrLn str))
        next

{-
実際に合成した関数の型はこうなるが実装はこれから説明
stdoutLogAndModularPlus
  :: Interpreter
    (LogF String :+: PlusF)
    ((StdoutLogInterpreter :&: ModularPlusInterpreter) IO)

A value for that last type isn't obvious. 
最後の型の値は明らかではない (上の型の定義だけあるので、その実装がないことについての説明？)
It ought to be automatic, somehow inferred by GHC given the two smaller Interpreters. 
2つの小さいインタープリターを与えることによりどうにかしてGHCに自動的に推論させるべきである
This calls for a typeclass which determines an Interpreter.
Interpreterの決定は型クラスによる
This class FInterpreter m b f can be read as "m interprets f in base monad b".
FInterpreter m b f クラスは mはbモナドを基にしたfにより解釈する と読める
-}

class Functor f => FInterpreter (m :: (* -> *) -> * -> *) (b :: * -> *) (f :: * -> *) where
    finterpret :: Interpreter f (m b)


{-
Now we need to give an instance for :&:. 
:&:のインスタンスが必要である
We want m :&: n to interpret a functor sum f :+: g over some base b, 
あるbを基にした f :+: g の合成されたファンクターを解釈できるm :&: nが必要である
but of course we have to qualify this with some assumptions:
しかし、もちろん、いくつかの仮定が必要である

.n interprets g over b
.nはbによってgを解釈する
.m interprets f over n b
.mはn bによってgを解釈する
.m, n give monads when stacked over b
.mとnはbにスタックされた時、モナドが与えられる
.m can be "stripped off" and then reset, as described by the class FTrans.
.mはFtransによってはぎ取り、リセットすることができる

That last assumption probably means that not all monad transformers can be used as interpreters, 
but it's nothing to worry about, because IdentityT and ReaderT r fulfill it, 
and these are just the monads we use for this demonstration.
最後の仮定おそらく全てのモナドトランスフォーマーがインタープリターとして使えるわけではないことを意味する
しかし、心配することはない、なぜなら、IdentityT と ReaderT r は適合している
そして、デモンストレーションに使ったモナドがちょうどそれである
-}

instance
    ( FInterpreter n  b    g
    , FInterpreter m (n b) f
    , Monad b
    , Monad (n b)
    , Monad (m (n b))
    , FTrans m
    ) => FInterpreter (m :&: n) b (f :+: g)
  where
    -- class Functor f => FInterpreter m b f where
    --       finterpret :: Interpreter f (m b )
    -- type Interpreter f m = forall a . f (m a ) -> m a
    -- data (f :+: g ) a = SumL (f a ) | SumR (g a )
    --         term = (:+:) f g ((:&:) m n b a)
    -- newtype (m :&: n) s t = Trans { runTrans :: m (n s ) t }
    finterpret term = case term of
        --   left = f ((:&:) m n b a)
        SumL left -> do
            -- runTrans = (:&:) m n b a -> m (n b ) a
            let term' = fmap runTrans left
            -- Trans      = m (n b ) a -> (:&:) m n b a
            -- finterpret = f (m (n b ) a ) -> m (n b ) a
            -- term'      = f (m (n b ) a )               
            Trans (finterpret term')
        --   right = g ((:&:) m n b a)
        SumR right -> do 
            -- outFTrans = (:&:) m n b ((:&:) m n b a -> n b a)
            outR <- outFTrans
            -- outR = (:&:) m n b a -> n b a
            let term' = fmap outR right
            -- inFTrans   = n b a -> (:&:) m n b a
            -- finterpret = g (n b a ) -> n b a
            -- term'      = g (n b a )
            inFTrans (finterpret term')

class FTrans (m :: (* -> *) -> * -> *) where
    outFTrans :: (Monad (m (n b)), Monad (n b)) => (m :&: n) b ((m :&: n) b t ->  n b t)
    inFTrans  :: (Monad (m (n b)))              =>               n b t        -> (m :&: n) b t

instance FTrans IdentityT where
    -- outFTrans    =                                                     (:&:) IdentityT  n b  ((:&:) IdentityT  n b  t -> n b t)
    -- Trans        = IdentityT (n b) ((:&:) IdentityT n b t -> n b t) -> (:&:) IdentityT  n b  ((:&:) IdentityT  n b  t -> n b t)
    -- IdentityT    =            n b  ((:&:) IdentityT n b t -> n b t) ->       IdentityT (n b) ((:&:) IdentityT  n b  t -> n b t)
    -- return       =                 ((:&:) IdentityT n b t -> n b t) ->                  n b  ((:&:) IdentityT  n b  t -> n b t)
    -- runIdentityT =                                                                                  IdentityT (n b) t -> n b t
    -- runTrans     =                  (:&:) IdentityT n b t ->                                        IdentityT (n b) t
    outFTrans = Trans (IdentityT (return (runIdentityT . runTrans)))
    -- inFTrans  =            n b  t -> (:&:) IdentityT  n b  t
    -- Trans     = IdentityT (n b) t -> (:&:) IdentityT  n b  t
    -- IdentityT =            n b  t ->       IdentityT (n b) t 
    inFTrans  = Trans . IdentityT

instance FTrans (ReaderT r) where
    -- outFTrans  =                                                          (:&:) (ReaderT r) n b  ((:&:) (ReaderT r) n b t -> n b t)
    -- Trans      = ReaderT r (n b ) ((:&:) (ReaderT r) n b  t -> n b t)  -> (:&:) (ReaderT r) n b  ((:&:) (ReaderT r) n b t -> n b t)
    -- ReaderT    =      (r -> n b   ((:&:) (ReaderT r) n b  t -> n b t)) ->        ReaderT r (n b) ((:&:) (ReaderT r) n b t -> n b t)
    -- return     =                  ((:&:) (ReaderT r) n b  t -> n b t)  ->                   n b  ((:&:) (ReaderT r) n b t -> n b t)
    -- x          =                   (:&:) (ReaderT r) n b  t
    -- runReaderT =                          ReaderT r (n b) t -> r -> n b t
    -- runTrans   =                   (:&:) (ReaderT r) n b  t -> ReaderT r (n b) t
    outFTrans = Trans (ReaderT (\r -> return (\x -> runReaderT (runTrans x) r)))
    -- inFTrans = n b t -> (:&:) (ReaderT r) n b t
    -- Trans    = ReaderT r (n b ) t -> (:&:) (ReaderT r) n b  t
    -- ReaderT  = (r     -> n b t )  ->        ReaderT r (n b) t
    -- const    =  n b t -> r        ->                   n b  t
    inFTrans  = Trans . ReaderT . const

{-
With FInterpreter and FTrans instances for our interpreters, we have enough machinery to give a value for stdoutLogAndModularPlus.
FInterpreterとFTransのインスタンスによるインタープリターによって、 stdoutLogAndModularPlusの実装を機械的に定義することができる
-}

instance Monad m => FInterpreter ModularPlusInterpreter m PlusF where
    finterpret = modularPlus

instance MonadIO m => FInterpreter StdoutLogInterpreter m (LogF String) where
    finterpret = stdoutLog

instance FTrans (ModularPlusInterpreter) where
    outFTrans = Trans (MPI (ReaderT (\r -> return (\x -> runReaderT (runMPI (runTrans x)) r))))
    inFTrans  = Trans . MPI . ReaderT . const

instance FTrans (StdoutLogInterpreter) where
    outFTrans = Trans (SLI (IdentityT (return (runIdentityT . runSLI . runTrans))))
    inFTrans  = Trans . SLI . IdentityT

stdoutLogAndModularPlus
  :: Interpreter
       (LogF String :+: PlusF)
       ((StdoutLogInterpreter :&: ModularPlusInterpreter) IO)
stdoutLogAndModularPlus = finterpret
{-
Actually, we don't need FTrans ModularPlusInterpreter, because it sits below (on the right of) StdoutLogInterpreter in the composite interpreter, but it's good to have anyway.
実際は、FTrans ModularPlusInterpreter は必要ない。なぜなら合成したインタープリター内のStdoutLogInterpreterの下(の右)に位置する
We can now interpret terms of Free (LogF :+: PlusF). This demands running every transformer in the stack, in-order, and stripping of the :&: constructors via runTrans, like so:
今、Free (LogF :+: PlusF) の解釈が可能である。スタック内の全てのトランスフォーマーの実行を要求するし  runTransで :&: をとる。下記のように

-}

step1 :: (:&:) StdoutLogInterpreter ModularPlusInterpreter IO Int
step1 = iterM stdoutLogAndModularPlus (logPlusTerm 5)

step2 :: ModularPlusInterpreter IO Int
step2 = runIdentityT . runSLI . runTrans $ step1

step3 :: Int -> IO Int
step3 i = (flip runReaderT) i . runMPI $ step2

{-
And that's it! Modular arithmetic with logging to stdout, arising from its parts.
これですべて! 剰余演算と標準出力へのログ出力はこの部分から生じる

I'm excited about this technique because it may open up a kind of normal form for programs,
in which all business logic is expressed by DSLs defined by functors, 
and these DSLs are mixed via functor sums to give the program's principal DSL, 
which is then interpreted by choosing interpreters for each part.
このテクニックに興奮している。なぜなら、普通のプログラミングの種類を広げてくれるからである
全てのビジネスロジックはファンクターとして定義されたDSLとして表現される
そしてそれらのDSLはプログラムの主要なDSLとして合成したファンクターになる
それぞれのインタープリターを選択して解釈する
-}

main :: IO ()
main = do
  putStrLn "end"
