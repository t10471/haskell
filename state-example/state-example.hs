import Control.Monad.State
import Data.Functor
import Control.Applicative
import Data.Char

type Op = State [Integer] Integer

push :: Integer -> Op
push c = do
  cs <- get
  put (c:cs)
  return c

pop :: Op
pop = do
  cs <- get
  put (tail cs)
  return (head cs)

up :: Integer -> Op
up i = do
  modify (map (i + ))
  return i

calc :: (Integer -> Integer -> Integer) -> Op
calc op = do
  i1 <- pop
  i2 <- pop
  push (op i1 i2)

-- Applicative style
calc' :: (Integer -> Integer -> Integer) -> Op
calc' op = op <$> pop <*> pop >>= push

add = calc (+)
sub = calc (-)
mul = calc (*)
dvv = calc div

poppp :: Op
poppp = do pop; pop; pop

pushshop :: Op
pushshop = do push 1; push 2; pop

type OpT = StateT [Integer] Maybe Integer

pushT :: Integer -> OpT
pushT a = do
    as <- get
    put (a:as)
    return a

popT :: OpT
popT = do
    as <- get
    f as
  where
    f []     = lift Nothing
    f (a:as) = do
        put as
        return a

calcT :: (Integer -> Integer -> Integer) -> OpT
calcT op = do
  i1 <- popT
  i2 <- popT
  pushT (op i1 i2)

addT = calcT (+)
subT = calcT (-)
mulT = calcT (*)
dvvT = calcT div

popppT :: OpT
popppT = do popT; popT; popT

popppT' :: OpT
popppT' = popT >>= \_ -> popT >>= \_ -> popT

main :: IO ()
main = do
  let stack = [1..5]
  print $ runState poppp stack
-- (3,[4,5])
  print $ runState pushshop  stack
-- (2,[1,1,2,3,4,5])
  print $ evalState poppp stack -- 値
-- 3
  print $ execState poppp stack -- 状態
-- [4,5]
  let a = state $ \s -> (1, s)  -- 関数から生成
  print $ runState a ()         -- 評価
-- (1,())
  print $ runState (up 5) stack
-- (5,[6,7,8,9,10])
  print $ runState add stack
-- (3,[3,3,4,5])
  print $ runStateT addT stack
-- Just (3,[3,3,4,5])
  print $ runStateT popppT stack
-- Just (3,[4,5])
  print $ runStateT (popppT >>= \_ -> addT) stack
-- Just (9,[9])
  print $ runStateT (popppT >>= \_ -> addT >>= \_ -> addT) stack
-- Nothing
  print $ runStateT (popppT `mplus` addT `mplus` addT) stack
-- Just (3,[4,5])


