{-# LANGUAGE Rank2Types, ViewPatterns #-}
import Control.Monad.Cont

-- 継続渡しモナド

calc1 :: Int -> Cont r Int
calc1 x = return (x + 3)

calc2 :: Int -> Cont r Int
calc2 x = return (x * 10)

calc3 :: Int -> Cont r Int
calc3 x = return (x + 4)

calcAll :: Int -> Cont r Int
calcAll x = return x >>= calc1 >>= calc2 >>= calc3

main1 :: IO ()
main1 = do
    -- a. 2 + 3 = 5
    runCont (calc1 2) print
    -- b. ((2 + 3) * 10) + 4 = 54
    runCont (calcAll 2) print
    -- 上記は以下と同じ
    -- runCont (calcAll 2) (\x -> print x)
    -- c. (((2 + 3) * 10) + 4) - 9 = 45
    print $ runCont (calcAll 2) (\x -> x - 9)
-- callCC は継続モナドの脱出機構を提供する
-- 以下の例では cc を呼ぶとそれ以降の処理をスキップする
sample2 :: Int -> Cont r Int
sample2 n = callCC $ \cc -> do
    when (odd n) $ do
        cc n        -- (1)
    return (n * 10) -- (2)

main2 :: IO ()
main2 = do
    runCont (sample2 1) print -- (1)
    runCont (sample2 2) print -- (2)
    runCont (sample2 3) print -- (1)
    runCont (sample2 4) print -- (2)

-- cc2 を読んでも cc1 の個所はスキップしない
sample3 :: Int -> Cont r Int
sample3 n = callCC $ \cc1 -> do
    when (odd n) $ do
        cc1 n                 -- (1)
    x <- callCC $ \cc2 -> do
        when (n <  4) $ do
            cc2 (n * 1000)    -- (2)
        when (n == 4) $ do
            cc1 (n * 100)     -- (3)
        return (n * 10)       -- (4)
    return (x + 1)            -- (5)

main3 :: IO ()
main3 = do
    runCont (sample3 1) print -- (1)
    runCont (sample3 2) print -- (2) (5)
    runCont (sample3 3) print -- (1)
    runCont (sample3 4) print -- (3)
    runCont (sample3 5) print -- (1)
    runCont (sample3 6) print -- (4) (5)
    runCont (return 4 >>= sample3) print
    -- 400
    runCont (return 4 >>= sample3 >>= sample3) print
    -- 4001
    runCont (return 4 >>= sample3 >>= sample3 >>= sample3) print
    -- 4001

newtype MaybeCPS a = MaybeCPS {unMaybeCPS :: forall r. (a -> Maybe r) -> Maybe r}

instance Monad MaybeCPS where
  return x = MaybeCPS $ \k -> k x
  MaybeCPS m >>= f = MaybeCPS $ \k -> m (\x -> unMaybeCPS (f x) k)
--
nothing :: MaybeCPS a
nothing = MaybeCPS $ (Nothing >>=)

hogeCPS :: Int -> MaybeCPS Int 
hogeCPS 0 = nothing
hogeCPS i = hogeCPS (i-1) >>= return

runMaybeCPS :: MaybeCPS r -> Maybe r
runMaybeCPS m = unMaybeCPS m  return

main :: IO ()
main = do
  main1
  main2
  main3
  print $ runMaybeCPS $ hogeCPS 1
  print $ runMaybeCPS $ hogeCPS 2
  print $ runMaybeCPS $ hogeCPS 3
