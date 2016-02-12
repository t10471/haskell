{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types, TupleSections, TypeOperators #-}

import Control.Object
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Skeleton
import qualified Data.Functor.Sum as F
data HumanState = HumanState
  { humanName :: String
  , humanOld :: Int
  }

data HumanObject a where
  GetName  :: HumanObject String
  GetOld   :: HumanObject Int
  Birthday :: HumanObject ()
  Greeting :: HumanObject ()

humanObject :: MonadIO m => String -> Int -> Object HumanObject m
humanObject n o = stateful handle (HumanState n o)
  where
    handle :: MonadIO m => HumanObject a -> StateT HumanState m a
    handle GetName  = get >>= return . humanName
    handle GetOld   = get >>= return . humanOld
    handle Birthday = do
      s <- get
      put $ s { humanOld = humanOld s + 1 }
    handle Greeting = do
      s <- get
      liftIO . putStrLn $ "Hello! I'm " 
        ++ humanName s ++ ", " ++ show (humanOld s) ++ " years old!"

ex1 :: IO ()
ex1 = do 
  h <- new . cascading $ humanObject "Yuzuko" 16
  h.- do
    bone Greeting 
    bone Birthday
    bone Birthday
    bone Greeting 

-- オーバーライド
-- メソッドをインターセプトすることで実現するが、IOがないので出力はできない
humanObjectPr :: (MonadIO m, Functor m) => String -> Int -> Object HumanObject m
humanObjectPr n o = liftO handle @>>@ cascading (humanObject n o)
  where
    -- IOをもっていない
    handle :: HumanObject a -> Skeleton HumanObject a
    handle Birthday = do
      -- 親のBirthdayメソッドを３回呼び出す
      bone Birthday 
      bone Birthday
      bone Birthday
    handle t = bone t

ex2 :: IO ()
ex2 = do 
  h <- new $ humanObjectPr "Kaede" 6
  h.-Greeting 
  h.-Birthday
  h.-Greeting 
  h.-Birthday
  h.-Greeting 

-- IOを伴ったオーバーライド
-- IOをつかうにはhandleの送信にIOがなければいけないが、
-- そのためには最終結果から持ってくる必要がある
-- humanObject :: Object HumanObject m なので受信にはIOがないので
-- echo :: Object m m と合成して Object (F.Sum HumanObject m) m
-- を作成することで最後の受信にIOが手に入りそれをもとにhandleでもIOがつかえるようになる
humanObjectJa :: (Functor m, MonadIO m) => String -> Int -> Object HumanObject m
humanObjectJa n o = liftO handle @>>@ cascading (humanObject n o @||@ echo)
  where
    handle :: (Functor m, MonadIO m) => HumanObject a -> Skeleton (F.Sum HumanObject m) a
    handle Greeting = do
      n <- bone $ F.InL GetName
      o <- bone $ F.InL GetOld
      bone . F.InR . liftIO . putStrLn
        $ "こんにちは！私の名前は" ++ n ++ "、" ++ show o ++ "歳です！"
    handle t = bone . F.InL $ t

ex3 :: IO ()
ex3 = do
  yuzuko <- new $ humanObjectJa "ゆずこ" 16
  yuzuko.-Greeting
  yuzuko.-Birthday
  yuzuko.-Greeting

liftO' :: Functor g => (forall x. f x -> g x) -> Object f g
-- (g x -> g (x, Object f g) ) -> (f x -> g x ) -> f x -> g (x, Object f g)
-- fmap  (\x -> (x, go))を先に計算するので fmapの第二引数は
-- g x でFunctorが必要
liftO' f = go where go = Object $ fmap (\x -> (x, go)) . f

-- 合成
data F a where
  FX :: F ()
  FY :: F ()

data G a where
  GX :: G ()
  GY :: G ()

-- 受ける方はFunctorでなければいけない
objX :: Object F (Skeleton G)
objX = liftO $ \case 
  FX -> bone GX
  FY -> bone GY

objY :: Object G IO
objY = liftO $ \case
  GX -> putStrLn "MessageGX"
  GY -> putStrLn "MessageGY"

-- 受ける方がSkeletonなので受け取る方もSkeletonにする
objZ :: Object F IO
objZ = objX @>>@ cascading objY

ex4 :: IO ()
ex4 = do
  y <- new objY
  y.-GX
  y.-GY

  putStrLn "----"
  z <- new objZ
  z.-FX
  z.-FY

data S a where
  GetS :: S Int
  SetS :: Int -> S ()
  PrintS :: S ()

objS :: Object S (StateT Int IO)
objS = liftO $ \case
    GetS   -> get
    SetS x -> put x
    PrintS -> get >>= liftIO . print

ex5 :: IO ()
ex5 = runStateT stateInt 100 >>= print

stateInt :: StateT Int IO ()
stateInt = do
  s <- new objS
  s.-PrintS
  s.-SetS 200
  s.-PrintS
  put 300
  s.-PrintS

-- variable :: Monad m => s -> Object (StateT s m) m
ex6 :: IO ()
ex6 = do
  v <- new $ variable "Hoge"
  -- vのインスタンスにStateTをメッセージとして送信
  v.- do
    x <- get
    put $ "~~~" ++ x ++ "~~~"
  -- 先ほど送ったメッセージにより、
  -- vの内部状態が書き換えられている
  a <- v.-get
  putStrLn a

objT :: Object S IO
objT = objS @>>@ variable 0

-- echo :: Functor f => Object f f
ex7 :: IO ()
ex7 = do
  -- IO上でインスタンス化されれば、
  -- 単純にIOをメッセージとして受け取り、そのまま送信するため、
  -- 以下のコードはただ単にHello, Worldを実行する
  e <- new echo
  e.-do
    putStrLn "Hello, World!"

objF :: Object F (Skeleton F)
objF = liftO $ \case
    FX -> do
    -- MessageXを二回送信
      bone FX
      bone FX
    -- MessageX以外のメッセージはそのまま
    t -> bone t

objZ' :: Object F IO
objZ' = objF @>>@ cascading objZ

ex8 :: IO ()
ex8 = do
  z1 <- new objZ
  z2 <- new objZ'
  invoke z1
  invoke z2
    where
      invoke :: Instance F IO -> IO ()
      invoke z = do
        putStrLn "----"
        z.-FX
        z.-FY


-- 継承
data A a where
  SetA   :: String -> A ()
  GetA   :: A String
  PrintA :: A ()

objA :: Object A IO
objA =  "" @~ \case 
  SetA s -> put s
  GetA   -> get
  PrintA -> get >>= liftIO . putStrLn . ("PrintA : "++)

data B a where
  PrintB  :: B ()
  PrintA2 :: B ()

-- objB :: Object (F.Sum A B) IO

mg :: Object B (Skeleton (F.Sum A IO))
mg = liftO $ \case
    PrintB  -> do
     x <- bone $ F.InL GetA
     bone . F.InR . putStrLn $ "PrintB : " ++ x
    PrintA2 -> do
     bone $ F.InL PrintA
     bone $ F.InL PrintA

mf :: Object A (Skeleton (F.Sum A IO))
mf = liftO $ \case
    x -> bone $ F.InL x

objA' :: Object (F.Sum A B) (Skeleton (F.Sum A IO))
objA' = mf @||@ mg

objA'' :: Object (F.Sum A IO) IO
objA'' = objA @||@ echo

objB :: Object (F.Sum A B) IO
objB = objA' @>>@ cascading objA''

ex9 :: IO ()
ex9 = do
  a <- new objA
  a.-SetA "Hoge"
  a.-PrintA 
  
  putStrLn "----"
  b <- new objB
  b.-F.InL (SetA "Piyo")
  b.-F.InL PrintA 
  b.-F.InR PrintB
  b.-F.InL (SetA "Fuga")
  b.-F.InR PrintA2

main :: IO ()
main = do
  putStrLn "end"
