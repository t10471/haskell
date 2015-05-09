import Control.Monad(Functor(..))
import Control.Applicative
import Data.Char
import Data.Traversable

-- char string parser
newtype P a = P { runP :: String -> [(a,String)] }

-- runP (P p) s = p s

instance Functor P where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (P q) = P (\s -> [ (f y,ys) | (y,ys) <- q s])

instance Applicative P where
  -- pure :: a -> f a
  pure x = P (\s -> [(x,s)])
  -- (<*>) :: f (a -> b) -> f a -> f b
  P p <*> P q = P (\s -> [(x y, ys) | (x,xs) <- p s, (y,ys) <- q xs])

letter = P p where      -- sample parser
  p (x:xs) | isAlpha x = [(x,xs)]
  p _ = []

instance Alternative P where
  -- (<|>) :: f a -> f a -> f a
  P p <|> P q = P (\s-> p s ++ q s)
  -- empty :: f a   -- the identity of <|>
  empty = P (\s-> [])

main :: IO ()
main = do
    print $ runP letter "123"
    -- []
    print $ runP letter "a123"
    -- [('a',"123")]
    print $ runP ( (:) <$> letter <*> ((:)<$>letter <*> pure []) ) "ab123"
    -- [("ab","123")]
    print $ runP ( (:) <$> letter <*> ((:)<$>letter <*> pure []) ) "a123"
    -- []
    print $ runP ( (:) <$> letter <*> pure []) "a123"
    -- [("a","123")]
    -- many の方が some より 1回多い
    -- のはマッチしてなくても実行するから？
    print $ runP (many letter) "ab123"
    -- [("ab","123"),("a","b123"),("","ab123")]
    print $ runP (some letter) "ab123"
    -- [("ab","123"),("a","b123")]

    -- optional 
    -- 引数の長さは関係ない？さいしょの文字だけ？
    print $ runP (optional letter) "ab123"
    -- [(Just 'a',"b123"),(Nothing,"ab123")]
    print $ runP (optional letter) "aba123"
    -- [(Just 'a',"ba123"),(Nothing,"aba123")]
    print $ runP (optional letter) "123"
    -- [(Nothing,"123")]
    print $ runP (optional letter) ""
    -- [(Nothing,"")]

    print $ runP (sequenceA $ replicate 2 letter) "ab123"
    -- [("ab","123")]

