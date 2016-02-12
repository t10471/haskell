{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
import Control.Monad.State
import Control.Lens
import Data.Functor.Identity

-- http://qiita.com/myuon_myon/items/38dc54565a37597ecf7e

data Autonomie m a = Autonomie { auto :: a, runAuto :: m () }

class Game c where
  update :: State c ()
  draw   :: StateT c IO ()

data Object = Object { _pos :: (Int, Int) }
makeClassy ''Object

type Character = Autonomie (State Object) Object

instance HasObject Character where
  -- object :: (Object -> f Object) -> Character -> f Character
  -- lens :: (s -> a ) -> (s -> b -> t ) -> Lens s t a b
  -- type Lens s t a b = forall f. Functor f =>  (a -> f b) -> s -> f t
  object = lens auto (\f a -> Autonomie a (runAuto f))

instance Game Character where
  update = do
    f <- get
    object %= execState (runAuto f)
  draw = do
    f <- get
    lift $ print $ f^.pos

walk :: State Object ()
walk = pos %= (\(x,y) -> (x+2, y))

dash :: State Object ()
dash = pos %= (\(x,y) -> (x+10, y))

main = do
  let chara1 = Autonomie (Object (100, 100)) walk
  draw `execStateT` (update `execState` chara1)

  let chara2 = Autonomie (Object (100, 100)) dash
  draw `execStateT` (update `execState` chara2)
