
import Control.Applicative

data Rectangle = Rectangle {_bottom :: Double, _hight :: Double} deriving Show
data Circle    = Circle    {_radius :: Double} deriving Show

class Figure a where
  area :: a -> Double 

instance Figure Rectangle where
  area x = _bottom x * _hight x

instance Figure Circle where
  area x = _radius x * _radius x * pi

type Holder a = ZipList a

total :: Figure a => Holder a -> Double
total m = foldl (\x y -> area y + x) 0 $ getZipList m

main :: IO ()
main = do
  print $ total $ pure Rectangle 1 3 <$> Rectangle 2 4
  print $ total $ pure Circle 3 <$> Circle 4
