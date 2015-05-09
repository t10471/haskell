{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Eff
import Control.Eff.Lift
import Data.Typeable
import Control.Applicative

data MyLog v = MyLog String v deriving (Functor, Typeable)


myLog :: Member MyLog r => String -> Eff r ()
myLog txt = send . inj $ MyLog txt ()

verboseAddition :: Member MyLog r => Eff r Int
verboseAddition = do
  myLog "I'm starting with 1..."
  x <- return 1
  myLog "and I'm adding 2..."
  y <- return 2
  let r = x + y
  myLog $ "Looks like the result is " ++ show r
  return r

runLogger :: Eff (MyLog :> r) a -> Eff r (a, [String])
runLogger = loop
  where
    first txt (v, l) = (v, txt:l)
    loop = freeMap
           (\x -> return (x, []))
           (\u -> handleRelay u loop
                  $ \(MyLog w v) -> first w <$> loop v)


-- runIOLogger :: Eff (MyLog :> Lift IO :> r) a -> Eff r a
runIOLogger :: SetMember Lift (Lift IO) r => Eff (MyLog :> r) a -> Eff r a
runIOLogger = loop 
  where
    lf m = send . inj $ Lift m id
    loop = freeMap 
           (\x -> return x)
           (\u -> handleRelay u loop 
                  $ \(MyLog w v) -> lf (putStrLn w) >> loop v)
           -- どうしてもliftが導出できない
           --        $ \(MyLog w v) -> lift (putStrLn w)  >> loop v)

main :: IO ()
main = do
  print $ run (runLogger verboseAddition)
  runLift $ runIOLogger verboseAddition
  putStrLn "end"
