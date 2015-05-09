
import Control.Monad.Free

data Greeting n = Old Int n | Hey n | Hello n | GoodBye 

instance Functor Greeting where
  fmap f (Old o n) = Old o (f n)
  fmap f (Hey n)   = Hey   (f n)
  fmap f (Hello n) = Hello (f n)
  fmap f GoodBye   = GoodBye

runGreeting :: Free Greeting r -> IO ()
runGreeting (Free (Old o n)) = putStrLn ("I'm " ++ show o ++ " years old") >> runGreeting n
runGreeting (Free (Hey n))   = putStrLn "Hey!!!"   >> runGreeting n
runGreeting (Free (Hello n)) = putStrLn "Hello!!!" >> runGreeting n
runGreeting (Free GoodBye)   = putStrLn "GoodBye!!!"

old :: Int -> Free Greeting ()
old o = liftF $ Old o ()

hey :: Free Greeting ()
hey = liftF $ Hey ()

hello :: Free Greeting ()
hello = liftF $ Hello ()

goodBye :: Free Greeting ()
goodBye = liftF GoodBye

--サブルーチン
sub :: Free Greeting ()
sub = do
  hello
  old 25

pg :: Free Greeting () 
pg = do
  hey
  sub --サブルーチン呼出し
  hey
  goodBye

greeting :: IO ()
greeting = do
  runGreeting $ Free (Hello (Free (Hello (Free GoodBye))))
  runGreeting $ (Free (Hello (Pure ()))) >> (Free GoodBye)
  runGreeting pg


main :: IO ()
main = do
  greeting


