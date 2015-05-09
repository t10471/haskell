import Control.Concurrent

-- <<modifyTwo
-- modifymvar modifyMVar_ は内部で mask を使って値を更新している
-- modifymvar は更新した値を返し、modifyMVar_ は返さない
modifyTwo :: MVar a -> MVar b -> (a -> b -> IO (a,b)) -> IO ()
modifyTwo ma mb f =
  -- MVar b -> (b -> IO b ) -> IO ()
  modifyMVar_ mb $ \b ->
    -- MVar a -> (a -> IO (a, b) ) -> IO b 
    modifyMVar ma $ \a -> f a b
-- >>

main = do
  ma <- newMVar 'a'
  mb <- newMVar 'b'
  modifyTwo ma mb (\a b -> return (succ a, succ b))
  readMVar ma >>= print
  readMVar mb >>= print
  modifyMVar ma (\a -> return('x', 'y')) >>= print
  -- => y
