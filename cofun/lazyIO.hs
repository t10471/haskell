import Data.IORef
import System.IO.Unsafe
import Debug.Trace

swap (x, y) = (y, x)
 
setup = do
  r1 <- newIORef True
  r2 <- newIORef True
  v1 <- unsafeInterleaveIO $ do writeIORef r2 False ; readIORef r1
  v2 <- unsafeInterleaveIO $ do writeIORef r1 False ; readIORef r2
  -- return $ trace ( "in setup " ++ show (v1,v2))  $ (v1, v2)
  return (v1, v2)
 
-- main = do
--   p1 <- setup
--   p2 <- setup
--   p3 <- setup
--   print p1
--   -- print p2
--   -- print p2
--   print . swap $ p2
--   -- print . swap $ trace ("debug " ++ show p2) $ p2
--   -- print . swap $ p3
--   -- print . swap $ p3
--   -- print p3

main = let m = setup
        in do p1 <- m
              p2 <- m
              print p1
              print . swap $ p2
