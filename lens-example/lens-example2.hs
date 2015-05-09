{-# LANGUAGE TemplateHaskell#-}

import Control.Lens
import Control.Lens.Action
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Monoid (Sum(..))
import Control.Lens.Extras (is)
import Data.Function
import Data.List.Lens
import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import System.Timeout (timeout)


data Foo a = Foo { _hoge :: a, _piyo :: Int  } deriving (Show, Eq) 
makeLenses ''Foo 
sampleFoo = Foo { _hoge = "Hello!", _piyo = 100 }

tm :: Int
tm = 5 * 10 ^ 6

timingOut :: NFData a => a -> IO a
timingOut = fmap (fromMaybe (error "timeout")) . timeout tm . evaluate . force

ex = do
  print $ sampleFoo ^. hoge
  print $ view hoge sampleFoo 
  print $ views hoge length sampleFoo

  print $ piyo .~ 999 $ sampleFoo 
  print $ sampleFoo & piyo .~ 999
  print $ set piyo 999 sampleFoo
  print $ sampleFoo & piyo <.~ 999
  -- (999,Foo {_hoge = "Hello!", _piyo = 999})

  print $ over piyo (+ 1) sampleFoo
  print $ sampleFoo & piyo %~ (+ 1)
  print $ sampleFoo & piyo <%~ (+ 1)
  -- (101,Foo {_hoge = "Hello!", _piyo = 101})

  traverseOf each print (1,2,3)
  (1,2,3) & each %%~ print

  print $ (1,2) & both +~ 1
  print $ (1,2) & _2 +~ 2
  print $ (1,2) & _2 <+~ 2
  print $ (1,2) & _2 -~ 2
  print $ (1,2) & _2 <-~ 2
  print $ (1,2) & _2 *~ 2
  print $ (1,2) & _2 <*~ 2
  print $ (1,2) & _2 //~ 2
  print $ (1,2) & _2 <//~ 2
  print $ (1,2) & _2 ^~ 2
  print $ (1,2) & _2 <^~ 2
  print $ (1,2) & _2 ^^~ (-2)
  print $ (1,2) & _2 <^^~ (-2)
  print $ (1,2) & _2 **~ 2
  print $ (1,2) & _2 <**~ 2

  print $ (False,True) & both ||~ True
  print $ (False,True) & _1 <||~ True
  print $ (False,True) & both &&~ True
  print $ (False,True) & _1 <&&~ True
  print $ (Sum 1,Sum 2) & both <>~ Sum 3
  print $ Left 4 ^? _Left
  print $ Left 4 ^?! _Left
  print $ (Right 4 ^? _Left :: Maybe Int)
  print $ "world" ^? ix 3
  print $ "world" ^? ix 20
  print $ [[1,2],[3]] ^.. traverse.traverse
  print $ (1,2) ^.. both
  print $ toListOf both (1,2)

  print $ has (element 0) []
  print $ has _Left (Left 12)
  print $ has _1 ("hello","world")
  print $ hasn't _Left (Right 12)
  print $ [1,2,3,4] ^.. folding tail
  print $ Just 3 ^.. folded
  print $ (Nothing ^.. folded :: [Maybe Int])
  print $ [(1,2),(3,4)] ^.. folded.both
  print $ 10 ^.. unfolded (\b -> if b == 0 then Nothing else Just (b, b-1))
  print $ [1..10] ^.. folded.filtered even
  a <- timingOut $ (5 :: Int) ^.. taking 20 repeated
  print a
  print $ 5 ^.. replicated 20
  a <- timingOut $ ([1,2,3] :: [Int]) ^.. taking 7 (cycled traverse)
  print a
  a <- timingOut $ toListOf (takingWhile (<=3) folded) ([1..] :: [Int])
  print a
  print $ toListOf (droppingWhile (<=3) folded) [1..6]

  print $ (1,2,3) & _2 %~ (* 100) 
  print $ (1,2,3) ^._2.to (*100)

  ["hello","world"] ^! folded.act putStrLn
  print $ ["ab","cd","ef"] ^!! folded.acts
  print $ [Just 1, Just 2, Just 3] ^!? folded.acts
  print $ [Just 1, Nothing] ^!? folded.acts


main :: IO ()
main = do
  putStrLn "end"
