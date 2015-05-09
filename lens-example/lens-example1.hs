{-# LANGUAGE TemplateHaskell#-}
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.State




data Man = Man { _name :: String, _age :: Int } deriving (Show, Eq, Ord)
emptyMan :: Man
emptyMan = Man { _name = "", _age = 0 }
makeLenses ''Man

exe1 :: IO ()
exe1 = do
  print $ hiratara ^. age
  putStrLn $ hiratara ^. name
  return ()
  where hiratara = (age .~ 36) . (name .~ "hiratara") $ emptyMan

exe2 :: IO ()
exe2 = flip evalStateT emptyMan $ do
  age .= 30
  name .= "hiratara"
  lift . print =<< use age
  lift . putStrLn =<< use name
  return ()

ex1 = do
  print $  ("Foo", "Bar", "Buz") ^. _1
  print $  ("Foo", "Bar", "Buz") ^. _2 
  print $  ("Foo", "Bar", "Buz") ^. _3
  print $  (100, 200, (310, (321, 322, 323, 999, 325), 330), 400) ^. _3 . _2 . _4
  print $  _2 .~ "Foo" $ (1, 2)
  print $  _2 .~ "Foo" $ (1, 2, 3)
  print $  _2 .~ "Foo" $ (1, 2, 3, 4)
  print $  _4 . _2 .~ 999 $ (1,2,3,(1,2,3),5)
  print $  (1,2,3,(1,2,3),5)　&　_4 . _2 .~ 999

data Point' = Point' { x' :: Int, y' :: Int } deriving (Show, Eq) 
data Line' = Line' { startPoint' :: Point'
                   , endPoint'   :: Point'
                 } deriving (Show, Eq)
sampleLine' = Line' { startPoint' = Point' { x' = 100, y' = 150 }
                    , endPoint'   = Point' { x' = 200, y' = 250 }
                    }

ex2 = do
  print $ endPoint' sampleLine'
  print $ sampleLine' { endPoint' = Point' { x' = 1000, y' = 1500 }}
  print $ x' . endPoint' $ sampleLine'
  print $ sampleLine' { endPoint' = (endPoint' sampleLine') { x' = 999 }}

data Point = Point { _x :: Int, _y :: Int } deriving (Show, Eq) 
makeLenses ''Point
data Line = Line { _startPoint :: Point, _endPoint :: Point } deriving (Show, Eq) 
makeLenses ''Line
sampleLine = Line { _startPoint = Point { _x = 100, _y = 150 }
                  , _endPoint   = Point { _x = 200, _y = 250 }
                  }

ex3 = do
  print $ sampleLine ^. startPoint 
  print $ sampleLine ^. endPoint
  print $ sampleLine ^. startPoint . x
  print $ sampleLine ^. endPoint   . y
  print $ sampleLine & startPoint . x .~ 999
  print $ sampleLine & endPoint   . x .~ 999

data Foo a = Foo { _hoge :: a, _piyo :: Int  } deriving (Show, Eq) 
makeLenses ''Foo 
sampleFoo = Foo { _hoge = "Hello!", _piyo = 100 }
ex4 = do
  print $ sampleFoo ^. hoge
  print $ sampleFoo & hoge .~ True
  print $ sampleFoo & piyo .~ 999

main :: IO ()
main = do
  putStrLn "end"
