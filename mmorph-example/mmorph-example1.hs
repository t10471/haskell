
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Morph
import Control.Exception

tick :: State Int ()
tick = modify (+1)
{-
tickにはIOがないが、、、
もともとtype State s = StateT s Identityなので
generalizeでIdentityを任意のモナドに変換する
generalize :: (Monad m) => Identity a -> m a
generalize m = return (runIdentity m)
hoistはfmap的な役割を持っていいる
hoist :: (Monad m, MFunctor t) => (forall a . m a -> n a) -> t m b -> t n b
hoist generalize :: (Monad m, MFunctor t) => t Identity b -> t m b
hoist generalize tick :: (Monad m) => StateT Int m () 
-}
tock                      ::                   StateT Int IO ()
tock = do
  hoist generalize tick   :: (Monad      m) => StateT Int m  ()
  lift $ putStrLn "Tock!" :: (MonadTrans t) => t          IO ()


-- ネストしたモナドの場合

-- 実際 :: StateT Int (WriterT [Int] Identity) ()
save    :: StateT Int (Writer  [Int]) ()
save = do
  n <- get
  lift $ tell [n]

program ::                   StateT Int (WriterT [Int] IO) ()
program = replicateM_ 4 $ do
  -- lift :: IO a -> t IO a
  hoist lift tock
      :: (MonadTrans t) => StateT Int (t             IO) ()
  hoist (hoist generalize) save
      :: (Monad      m) => StateT Int (WriterT [Int] m ) ()

exec = execWriterT (runStateT program 0) >>= print

-- embedを使用する例

check :: IO a -> ExceptT IOException IO a
check io = ExceptT (try io)

program2 :: ExceptT IOException IO ()
program2 = do
  str <- lift $ readFile "test.txt"
  check $ putStr str
  -- 本来は以下とすべきところ
  -- str <- check $ readFile "test.txt"
  -- lift $ putStr str

exece = runExceptT program2

{-
上野プログラムを書き換えないで処理する
checkはIdentityではないしhoistを使ってもうまくいかない
hoist check :: (MFunctor t) => t IO a -> t (ExceptT IOException IO) a
hoist check program :: ExceptT IOException (ExceptT IOException IO) ()
embedを使用すると型を変えずに処理を追加出来る
embed check :: ExceptT IOException IO a -> ExceptT IOException IO a
embed check program :: ExceptT IOException IO ()
-}
exec2 = do
  r <- runExceptT (embed check program2)
  case r of 
    Left e  -> putStrLn $ show e
    Right a -> putStrLn "file exist"

main :: IO ()
main = do
  x <- runStateT tock 0
  print x
  putStrLn "end"
