-- module Transformers where
import Control.Monad.Identity
import Control.Monad.Trans.Except 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name   = String                -- variable names (変数名)
-- expressions (式)
data Exp    = Lit Integer           -- 整数リテラル
            | Var Name              -- 変数
            | Plus Exp Exp          -- 加算
            | Abs Name Exp          -- 抽象     評価される環境にを捉えて関数値に評価する
            | App Exp Exp           -- 関数適用 最初に関数と引数を評価し、加算と同じように処理
            deriving (Show)
data Value  = IntVal Integer        -- values (値)
            | FunVal Env Name Exp
            deriving (Show)
type Env    = Map.Map Name Value    -- mapping from names to values (変数名から値へのマッピング)

evalSimple :: Env -> Exp -> Value
evalSimple env (Lit i)       = IntVal i
evalSimple env (Var n)       = fromJust (Map.lookup n env)
evalSimple env (Plus e1 e2)  = let IntVal i1 = evalSimple env e1
                                   IntVal i2 = evalSimple env e2
                               in  IntVal (i1 + i2)
evalSimple env (Abs n e)     = FunVal env n e
evalSimple env (App e1 e2)   = let val1 = evalSimple env e1
                                   val2 = evalSimple env e2
                               in case val1 of
                                FunVal env' n body -> evalSimple (Map.insert n val2 env') body

-- Indentity Version

type EvalI a = Identity a
runEvalI :: EvalI a -> a
runEvalI ev = runIdentity ev

evalI :: Env -> Exp -> EvalI Value
evalI env (Lit i)       = return $ IntVal i
evalI env (Var n)       = maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
evalI env (Plus e1 e2)  = do IntVal i1 <- evalI env e1
                             IntVal i2 <- evalI env e2
                             return $ IntVal (i1 + i2)
evalI env (Abs n e)     = return $ FunVal env n e
evalI env (App e1 e2)   = do val1 <- evalI env e1
                             val2 <- evalI env e2
                             case val1 of
                                FunVal env' n body ->
                                    evalI (Map.insert n val2 env') body

-- ExceptT Version

type EvalE a = ExceptT String Identity a
runEvalE :: EvalE a -> Either String a
runEvalE ev = runIdentity (runExceptT ev)

evalE :: Env -> Exp -> EvalE Value
evalE env (Lit i)       = return $ IntVal i
evalE env (Var n)       = case Map.lookup n env of
                            Nothing  -> throwE ("unbound variable: " ++ n)
                            Just val -> return val
evalE env (Plus e1 e2)  = do e1' <- evalE env e1
                             e2' <- evalE env e2
                             case (e1', e2') of
                                (IntVal i1, IntVal i2)
                                    -> return $ IntVal (i1 + i2)
                                _   -> throwE "type error in addition"
evalE env (Abs n e)     = return $ FunVal env n e
evalE env (App e1 e2)   = do val1 <- evalE env e1
                             val2 <- evalE env e2
                             case val1 of
                                FunVal env' n body
                                    -> evalE (Map.insert n val2 env') body
                                _   -> throwE "type error in application"

-- ReaderT を使うことで evalR には引数に環境がない 
-- runEvalR 実行時に渡す
-- ask を使って環境を取得
-- local を使って環境を更新しながら再帰呼び出しをしている 
-- local は一時的に環境を変更する
-- local :: (r -> r) -> m a -> m a
-- local (+ 1)　(reader (\_ -> 2)) :: (MonadReader r m, Num a, Num r) => m a
-- runReader (local (+ 1)　(reader (\_ -> 2))) 3 => 2 local の影響なし
-- const (Map.insert 1 2 Map.empty) :: (Ord k, Num a, Num k) => b -> Map.Map k a
-- local でもらう内部の環境が必要ないので、捨てるために const
-- でくくっている 

type EvalR a = ReaderT Env (ExceptT String Identity) a
runEvalR :: Env -> EvalR a -> Either String a
runEvalR env ev = runIdentity (runExceptT (runReaderT ev env))

evalR :: Exp -> EvalR Value
evalR (Lit i)       = return $ IntVal i
evalR (Var n)       = do env <- ask
                         case Map.lookup n env of
                            Nothing  -> lift $ throwE ("unbound variable: " ++ n)
                            Just val -> return val
evalR (Plus e1 e2)  = do e1' <- evalR e1
                         e2' <- evalR e2
                         case (e1', e2') of
                            (IntVal i1, IntVal i2)
                                -> return $ IntVal (i1 + i2)
                            _   -> lift $ throwE "type error in addition"
evalR (Abs n e)     = do env <- ask
                         return $ FunVal env n e
evalR (App e1 e2)   = do val1 <- evalR e1
                         val2 <- evalR e2
                         case val1 of
                            FunVal env' n body
                                -> local (const (Map.insert n val2 env')) (evalR body)
                            _   -> lift $ throwE "type error in application"

-- StateT をつけ処理回数をカウントするように修正
-- EvalS  EvalS' は順序を入れ替え
-- EvalS  => (Right (IntVal 18),8)
-- EvalS' => Right (IntVal 18,8)

type EvalS a = ReaderT Env (ExceptT String (StateT Integer Identity)) a
runEvalS :: Env -> Integer -> EvalS a -> (Either String a, Integer)
runEvalS env st ev = runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

type EvalS' a = ReaderT Env (StateT Integer (ExceptT String Identity)) a
runEvalS' :: Env -> Integer -> EvalS' a -> (Either String (a, Integer))
runEvalS' env st ev = runIdentity (runExceptT (runStateT (runReaderT ev env) st))

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

evalS :: Exp -> EvalS Value
evalS (Lit i)       = do tick
                         return $ IntVal i
evalS (Var n)       = do tick
                         env <- ask
                         case Map.lookup n env of
                            Nothing  -> lift $ throwE ("unbound variable: " ++ n)
                            Just val -> return val
evalS (Plus e1 e2)  = do tick
                         e1' <- evalS e1
                         e2' <- evalS e2
                         case (e1', e2') of
                            (IntVal i1, IntVal i2)
                                -> return $ IntVal (i1 + i2)
                            _   -> lift $ throwE "type error in addition"
evalS (Abs n e)     = do tick
                         env <- ask
                         return $ FunVal env n e
evalS (App e1 e2)   = do tick
                         val1 <- evalS e1
                         val2 <- evalS e2
                         case val1 of
                            FunVal env' n body
                                -> local (const (Map.insert n val2 env')) (evalS body)
                            _   -> lift $ throwE "type error in application"


evalS' :: Exp -> EvalS' Value
evalS' (Lit i)      = do tick
                         return $ IntVal i
evalS' (Var n)      = do tick
                         env <- ask
                         case Map.lookup n env of
                            Nothing  -> lift $ lift $ throwE ("unbound variable: " ++ n)
                            Just val -> return val
evalS' (Plus e1 e2) = do tick
                         e1' <- evalS' e1
                         e2' <- evalS' e2
                         case (e1', e2') of
                            (IntVal i1, IntVal i2)
                                -> return $ IntVal (i1 + i2)
                            _   -> lift $ lift $ throwE "type error in addition"
evalS' (Abs n e)    = do tick
                         env <- ask
                         return $ FunVal env n e
evalS' (App e1 e2)  = do tick
                         val1 <- evalS' e1
                         val2 <- evalS' e2
                         case val1 of
                            FunVal env' n body
                                -> local (const (Map.insert n val2 env')) (evalS' body)
                            _   -> lift $ lift $ throwE "type error in application"

-- WriterT で変数のログを記述する Version
-- tell :: w -> Writer w ()
-- modify に近い

type EvalW a = ReaderT Env (ExceptT String
                           (WriterT [String] (StateT Integer Identity))) a
runEvalW :: Env -> Integer -> EvalW a -> ((Either String a, [String]), Integer)
runEvalW env st ev =
      runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

evalW :: Exp -> EvalW Value
evalW (Lit i)   = do tick
                     return $ IntVal i
evalW (Var n)   = do tick
                     tell [n]
                     env <- ask
                     case Map.lookup n env of
                        Nothing  -> lift $ throwE ("unbound variable: " ++ n)
                        Just val -> return val
evalW (Plus e1 e2) = do tick
                        e1' <- evalW e1
                        e2' <- evalW e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2)
                                -> return $ IntVal (i1 + i2)
                            _   -> lift $ throwE "type error in addition"
evalW (Abs n e)   = do tick
                       env <- ask
                       return $ FunVal env n e
evalW (App e1 e2) = do tick
                       val1 <- evalW e1
                       val2 <- evalW e2
                       case val1 of
                        FunVal env' n body
                            -> local (const (Map.insert n val2 env')) (evalW body)
                        _   -> lift $ throwE "type error in application"

type EvalIO a = ReaderT Env (ExceptT String
                           (WriterT [String] (StateT Integer IO))) a

-- IO Version
-- liftIO でIOに持ち上げている

runEvalIO :: Env -> Integer -> EvalIO a -> IO ((Either String a, [String]), Integer)
runEvalIO env st ev
    = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

evalIO :: Exp -> EvalIO Value
evalIO (Lit i)      = do tick
                         liftIO $ print i
                         return $ IntVal i
evalIO (Var n)      = do tick
                         tell [n]
                         env <- ask
                         case Map.lookup n env of
                             Nothing  -> lift $ throwE ("unbound variable: " ++ n)
                             Just val -> return val
evalIO (Plus e1 e2) = do tick
                         e1' <- evalIO e1
                         e2' <- evalIO e2
                         case (e1', e2') of
                             (IntVal i1, IntVal i2)
                                 -> return $ IntVal (i1 + i2)
                             _   -> lift $ throwE "type error in addition"
evalIO (Abs n e)    = do tick
                         env <- ask
                         return $ FunVal env n e
evalIO (App e1 e2)  = do tick
                         val1 <- evalIO e1
                         val2 <- evalIO e2
                         case val1 of
                             FunVal env' n body
                                 -> local (const (Map.insert n val2 env')) (evalIO body)
                             _   -> lift $ throwE "type error in application"

-- 12 + ((λx -> x)(4+2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

main :: IO ()
main = do
  print $ evalSimple Map.empty exampleExp
  print $ runEvalI $ evalI Map.empty exampleExp
  print $ runEvalE $ evalE Map.empty exampleExp
  print $ runEvalE $ evalE Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))
  print $ runEvalE $ evalE Map.empty (Var "x")
  print $ runEvalR Map.empty $ evalR exampleExp
  print $ runEvalS Map.empty 0 $ evalS exampleExp
  print $ runEvalS' Map.empty 0 $ evalS' exampleExp
  print $ runEvalW Map.empty 0 $ evalW exampleExp
  runEvalIO Map.empty 0 $ evalIO exampleExp
  putStrLn "end"

