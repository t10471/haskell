{-# LANGUAGE TemplateHaskell#-}

import Control.Lens
import Control.Monad.State

data Ball = Ball { _posBall :: (Int, Int), _velBall :: (Int, Int), _r :: Int } deriving (Eq, Show)
makeLenses ''Ball

data Bar = Bar { _posBar :: (Int, Int), _velBar :: (Int, Int), _width :: Int, _height :: Int } deriving (Eq, Show)
makeLenses ''Bar

class HasPos c where
  poss :: Lens' c (Int, Int)

instance HasPos Ball where
  poss = posBall
instance HasPos Bar where
  poss = posBar

reset :: (HasPos c) => State c ()
reset = do
  poss .= (0,0)

data GameObj = GameObj { _pos :: (Int, Int), _vel :: (Int, Int) } deriving (Eq, Show)
makeClassy ''GameObj
{-
以下が自動生成される
class HasGameObj c where
  gameObj :: Lens' c GameObj

instance HasGameObj GameObj where
  gameObj = id

pos :: (HasGameObj c) => Lens' c (Int, Int)
vel :: (HasGameObj c) => Lens' c (Int, Int)
-}
data Block = Block { _blockObj :: GameObj, _x :: Int } deriving (Eq, Show)
makeLenses ''Block

data Enemy = Enemy { _enemyObj :: GameObj, _y :: Int } deriving (Eq, Show)
makeLenses ''Enemy

instance HasGameObj Block where
  gameObj = blockObj

instance HasGameObj Enemy where
  gameObj = enemyObj

makeGameObj = GameObj (0,0) (0,0)
makeBlock = Block makeGameObj 0
makeEnemy = Enemy makeGameObj 0

main :: IO ()
main = do 
  print $ makeBlock ^. vel
  print $ makeBlock ^. pos
  print $ makeEnemy & vel .~ (10,10)
  putStrLn "end"
