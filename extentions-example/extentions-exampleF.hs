{-# LANGUAGE ExistentialQuantification #-}

-- ExistentialQuantificationを使った多相リストに近いもの

class Shape a where
    display :: a -> String

data Triangle = Triangle deriving Show
instance Shape Triangle where
    display _ = "  *  \n" ++
                " * * \n" ++
                "*****\n"

data Rectangle = Rectangle deriving Show
instance Shape Rectangle where
    display _ = "*****\n" ++
                "*   *\n" ++
                "*****\n"

-- data に forall がかけるが deriving は使えない
-- newtype には使えない
data Polygon = forall a . (Shape a, Show a) => Polygon a

instance Shape Polygon where
    display (Polygon x) = display x

instance Show Polygon where
    show (Polygon a) = "Polygon " ++ show a

main :: IO ()
main = do
    mapM_ putStr $ map display [Polygon Triangle, Polygon Rectangle]
