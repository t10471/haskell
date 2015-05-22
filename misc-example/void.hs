
import Data.Void

-- absurd は絶対に通らないけど型を合わせるためだけに必要？

data RuleSet a            = Known !a | Unknown String
data GoRuleChoices        = Japanese | Chinese
type GoRuleSet            = RuleSet GoRuleChoices
type LinesOfActionRuleSet = RuleSet Void

handleLOARules :: (String -> a) -> LinesOfActionRuleSet -> a
handleLOARules f r = case r of
  Known   a -> absurd a
  Unknown s -> f s

simple :: Either Void a -> a
simple (Left x) = absurd x
simple (Right y) = y

data Pipe a b r
  = Pure r
  | Await (a -> Pipe a b r)
  | Yield !b (Pipe a b r)

type Consumer a r = Pipe a Void r

foldConsumer :: (r -> s) -> ((a -> s) -> s) -> Consumer a r -> s
foldConsumer onPure onAwait p 
 = case p of
     Pure x    -> onPure x
     Await f   -> onAwait $ \x -> foldConsumer onPure onAwait (f x)
     Yield x _ -> absurd x

main :: IO ()
main = do
  putStrLn "end"
