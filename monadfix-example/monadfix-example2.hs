{-# LANGUAGE RecursiveDo #-}

import Data.IORef
import Control.Monad.Fix
-- 相互再帰
-- 相互に依存している変数を遅延評価し相互に利用している再帰
-- RecursiveDo は mdo と rec を使用することができる
-- mdo と rec を使用するには MonadFix を実装している必要がある
-- rec { b <- f a c     ===>    (b,c) <- mfix (\~(b,c) -> do { b <- f a c
--     ; c <- f b a }                                        ; c <- f b a
-- mdo { a <- getChar      ===> do { a <- getChar
--     ; b <- f a c                ; rec { b <- f a c
--     ; c <- f b a                ;     ; c <- f b a }
--     ; z <- h a b                ; z <- h a b
--     ; d <- g d e                ; rec { d <- g d e
--     ; e <- g a z                ;     ; e <- g a z }
--     ; putChar c }               ; putChar c }

data Node = Node Int (IORef Node)

nodemain f = do
  p <- f
  Node x q <- readIORef p
  print x
  Node y _ <- readIORef q
  print y

-- with rec-syntax
recnode = do
    rec p <- newIORef (Node 0 p)
    putStrLn "node created"
    return p
  
-- with mfix
mfixnode = mfix (\p -> do
    p' <- newIORef (Node 0 p)
    putStrLn "node created"
    return p')

-- with mdo-syntax
mdonode = mdo
  p <- newIORef (Node 0 p)
  putStrLn "node created"
  return p

-- with rec-syntax
recnodes = do
    rec p <- newIORef (Node 0 r)
        r <- newIORef (Node 1 p)
    putStrLn "nodes created"
    return p

-- with mfix
mfixnodes = mfix (\ ~(p,r) -> do
    p' <- newIORef (Node 0 r)
    r' <- newIORef (Node 1 p')
    putStrLn "nodes created"
    return (p',r'))
  >>= \(p,r) -> return p

-- with mdo-syntax
mdonodes = mdo
  p <- newIORef (Node 0 r)
  r <- newIORef (Node 1 p)
  putStrLn "nodes created"
  return p 

data BTree = Z | B Int BTree BTree deriving Show

rep_x_sum Z _ = return (Z, 0)
rep_x_sum (B i l r) s = do
  putStr "("
  (l',sl) <- rep_x_sum l s
  putStr (show i)
  (r',sr) <- rep_x_sum r s
  putStr ")"
  return (B s l' r', i + sl + sr)

repsummain f = f (B 4 (B 3 Z Z) (B 5 Z (B 1 Z Z)))
       >>= print

-- with rec-syntax
recrepsum t = do
    rec (u,s) <- rep_x_sum t s
    putStrLn ""
    return u 
       
-- with mfix
mfixrepsum t = mfix (\ ~(u,s) -> do
    (u',s') <- rep_x_sum t s
    putStrLn ""
    return (u',s'))
  >>= \(u,s) -> return u

-- with mdo-syntax
mdorepsum t = mdo
    (u,s) <- rep_x_sum t s
    putStrLn ""
    return u 

       
data BTreeIO = ZIO | BIO (IORef Int) BTreeIO BTreeIO

iorep_x_sum ZIO _ = return 0
iorep_x_sum (BIO ref l r) s = do
  i <- readIORef ref
  writeIORef ref s
  putStr "("
  sl <- iorep_x_sum l s
  putStr (show i)
  sr <- iorep_x_sum r s
  putStr ")"
  return (i + sl + sr)
-- with rec-syntax

iorepsummain f = do
  r4 <- newIORef 4
  r3 <- newIORef 3
  r5 <- newIORef 5
  r1 <- newIORef 1
  let t = (BIO r4 (BIO r3 ZIO ZIO) (BIO r5 ZIO (BIO r1 ZIO ZIO)))
  f t
  f t

-- with rec-syntax
reciorepsum t = do
    rec s <- iorep_x_sum t s
    putStrLn ""
    return ()
 
-- with mfix
mfixiorepsum t = mfix (\s -> do
    s' <- iorep_x_sum t s
    putStrLn ""
    return s')
  >> return ()

-- with mdo-syntax
mdoiorepsum t = mdo
    s <- iorep_x_sum t s
    putStrLn ""
    return ()

main :: IO ()
main = do
  nodemain recnode
  nodemain mfixnode
  nodemain mdonode
  nodemain recnodes
  nodemain mfixnodes
  nodemain mdonodes
  repsummain recrepsum
  repsummain mfixrepsum
  repsummain mdorepsum
  iorepsummain reciorepsum
  iorepsummain mfixiorepsum
  iorepsummain mdoiorepsum

