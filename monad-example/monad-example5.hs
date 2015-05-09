import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Trans
import System.Random.MWC

type ITuple = (Int, Int)
type Condition = [ITuple -> Bool]
data Problem =  Problem { iTuple    :: [ITuple]
                        , condition :: Condition
                        , seed      :: Seed
                        }
type ProblemSTT a = StateT Problem [] a
rdmR :: Variate a => (a, a) -> Seed -> (a, Seed)
rdmR range seed = runST $ do
    gen   <- restore seed
    ret   <- uniformR range gen
    seed' <- save gen
    return (ret, seed')

getOne :: Variate a => (a,a) -> State Seed a
getOne bounds = do 
    g      <- get
    (x,g') <- return $ rdmR bounds g
    put g'
    return x

makeITuple :: Seed -> (ITuple, Seed)
makeITuple = runState $ do 
    i <- getOne (1,100) 
    j <- getOne (1,100) 
    return (i, j)

convetMIT :: Seed -> ProblemSTT (ITuple, Seed)
convetMIT s = return $ makeITuple s

genITuple :: ProblemSTT ()
genITuple = do
    p <- get
    (t, s) <- convetMIT $ seed p
    put $ p {iTuple = t : (iTuple p), seed = s}

conditionD :: Condition
conditionD =  [ (\(x,y) -> x `mod` 3 == 0)
              , (\(x,y) -> y `mod` 2 == 0)
              ]

testD :: [ITuple]
testD = [(1,2),(2,3),(3,2),(5,4)]

inSolve :: [ITuple] -> Condition -> [ITuple]
inSolve is cs = do
    loop is cs []
  where
    loop []     _      r  = r
    loop (x:xs) []     r  = loop xs cs (x:r)
    loop (x:xs) (y:ys) r  = if y x 
                            then loop (x:xs) ys r 
                            else loop xs cs r

genCondition :: ProblemSTT ()
genCondition = do
    p <- get
    put $ p {condition = conditionD}

solve:: ProblemSTT [ITuple]
solve = do
    p <- get
    let cs = condition p
    let is = iTuple p
    return $ inSolve is cs
  
setupProblem :: Seed -> Problem
setupProblem s = Problem [] [] s

evalStateLT :: Seed -> [ITuple]
evalStateLT s = concat $ flip evalStateT (setupProblem s) $ do
    replicateM_ 10 genITuple
    genCondition
    solve

exStateLT :: IO ()
exStateLT = do
    s <- createSystemRandom >>= save
    print $ evalStateLT s
    return ()

main :: IO ()
main = do
    exStateLT
    putStrLn "end"
