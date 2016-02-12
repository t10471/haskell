{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Writer
import Data.String
import Control.Monad.Cont
import Data.Functor.Identity

-- http://chrisdone.com/posts/haskell-constraint-trick
-- data MyTuple a b = MyTuple a b
-- instance Show (MyTuple a b) where
--     show _ = "MyTuple <some value> <some value>"

-- instance (Show a,Show b) => Show (MyTuple a b) where
--     show (MyTuple a b) = "MyTuple " ++ show a ++ " " ++ show b

instance a ~ () => IsString (Writer String a) where
    fromString = tell

-- main :: IO ()
-- main = do
--   -- print $ MyTuple "a" "b"
--   -- うまくいくはずがいかない
--   print $ execWriter (do "hello"; "world" :: Writer String ())

-- http://unbui.lt/#!/post/haskell-cont-monad
--

twoC' :: Cont a Int
twoC' = return 2

helloC' :: Cont a String
helloC' = return "hello"

run :: Cont r r -> (r -> r ) -> r
run f = runCont f id

twoMultiC = do
    two <- twoC'
    hello <- cont $ \out -> out "hello" ++ out "hello"
    return $ (show two) ++ hello

twoMultiC' = do
    two <- cont $ \out -> out 2 ++ out 2
    hello <- helloC'
    return $ (show two)++hello

twoMultiC'' = do
    two <- twoC'
    hello <- helloC'
    cont $ \out -> out ((show two)++hello) ++ out ((show two)++hello)

desugaredTwoMultiC =
    (cont $ \out1 -> out1 2 ++ out1 2) >>= \two ->
        (cont $ \out2 -> out2 "hello") >>= \hello ->
            (cont $ \out3 -> out3 $ (show two)++hello)

desugaredTwoMultiC' =
    (cont $ \out1 -> out1 "hello") >>= \hello ->
      (cont $ \out2 -> out2 2 ++ out2 2) >>= \two ->
            (cont $ \out3 -> out3 $ (show two)++hello)

boom1C = do
    n <- cont $ \out -> "boom! "
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our first line never calls out, so we just return the string:
-- runCont boom1C id == "boom! "

boom2C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> "boom! "
    x <- cont $ \out -> out "X" ++ out "Y"
    return $ n++l++x++" "

-- here, our we call out twice, each one hitting the second continuation and
-- exiting with boom, so we get two of them appended because out "1" and out "2" are:
-- runCont boom2C id == "boom! boom! "

boom3C = do
    n <- cont $ \out -> out "1" ++ out "2"
    l <- cont $ \out -> out "a" ++ out "b"
    x <- cont $ \out -> "boom! "
    return $ n++l++x++" "

-- each contnuation calls its callback twice, so we end up hitting boom 4 times:
-- runCont boom3C id == "boom! boom! boom! boom! "


class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))


main = do
  print $ run twoMultiC
  print $ run twoMultiC'
  print $ run twoMultiC''
  print $ run desugaredTwoMultiC
  print $ run desugaredTwoMultiC'
  print $ run boom1C
  print $ run boom2C
  print $ run boom3C
  putStrLn "end"



