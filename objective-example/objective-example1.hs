{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor    #-}

import Control.Object
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import qualified Data.Functor.Sum as Functor

{-
newtype Object f g = Object { runObject :: forall x. f x -> g (x, Object f g) }

f が受信するメッセージの型 gが送信するメッセージの型

new  :: MonadIO m => Object f g -> m (Instance f g)
(.-) :: (MonadIO m, Control.Monad.Catch.MonadMask m) => Instance f m -> f a -> m a
-}

-- 型がインターフェースとなる
data StringObject a where
  GetString   :: StringObject String
  SetString   :: String -> StringObject ()
  PrintString :: StringObject ()

{-
stringObject :: MonadIO m =>  String -> Object StringObject m
stringObject s = stateful handle s
  where
    handle :: MonadIO m => StringObject a -> StateT String m a
    handle GetString     = get
    handle (SetString s) = put s
    handle PrintString   = get >>= liftIO . putStrLn
-}
-- LambdaCaseを使ったversion @~はstatefulの引数が逆 version
-- 型に合わせた実装を定義する
stringObject :: MonadIO m =>  String -> Object StringObject m
stringObject s = s @~ \case
  GetString   -> get
  SetString s -> put s
  PrintString -> get >>= liftIO . putStrLn

ex1 :: IO ()
ex1 = do 
  -- インスタンス生成
  str1 <- new $ stringObject "Hoge"
  -- メソッド呼び出し
  str1.-PrintString
  str1.-SetString "Foo"
  str1.-PrintString
  -- 取得
  x <- str1.-GetString
  putStrLn $ "x = " ++ x

-- 別の実装
namedStrObject :: MonadIO m => String -> String -> Object StringObject m
namedStrObject n s = stateful handle s
  where
    handle :: MonadIO m => StringObject a -> StateT String m a
    handle GetString     = get
    handle (SetString s) = put s
    handle PrintString   = get >>= liftIO . putStrLn . ((n ++ ": ") ++)

ex2 :: IO ()
ex2 = do
  str1 <- new $ stringObject "Hoge"
  str2 <- new $ namedStrObject "Tune" "Hoge" 
  invoke str1
  invoke str2
    where
      invoke :: Instance StringObject IO -> IO ()
      invoke obj = do
        obj.-PrintString
        obj.-SetString "Piyo"
        obj.-PrintString

-- レコードを持ったパターン

data HumanState = HumanState
  { humanName :: String
  , humanOld  :: Int
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

ex3 :: IO ()
ex3 = do
  yuzuko <- new $ humanObject "Yuzuko" 16
  yuzuko.-Greeting
  yuzuko.-Birthday
  yuzuko.-Greeting

{-
オブジェクトの合成

(@||@) :: Functor m => Object f m -> Object g m -> Object (Sum f g) m
a @||@ b = Object $ \r -> case r of
  InL f -> fmap (fmap (@||@ b)) $ runObject a f
  InR g -> fmap (fmap (a @||@)) $ runObject b g
-}

humanObjectGe :: (Functor m, MonadIO m) => 
  String -> Int -> String -> Object (Functor.Sum HumanObject StringObject) m
humanObjectGe n o s = humanObject n o @||@ namedStrObject n s

ex4 :: IO ()
ex4 = do
  h <- new $ humanObjectGe "Yuzuko" 16 "tune is nice guy."
  h.-Functor.InL Greeting
  h.-Functor.InL Birthday
  h.-Functor.InL Greeting
  h.-Functor.InR PrintString
  h.-Functor.InR (SetString "haskell is cool!")
  h.-Functor.InR PrintString


main :: IO ()
main = do
  putStrLn "end"
