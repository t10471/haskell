--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--
--ファイル一覧
-- parinfer.hs    メイン
-- FiniteMap.hs
-- InferMonad.hs
-- StateX.hs
-- MyList.hs       minusの定義(配列から値を削除する関数)
-- MaybeM.hs
-- Lex.hs          alexで生成した字句解析ルール
-- Substitution.hs aaa
-- Term.hs         VarId,Termの定義,freeVars(自由変数一覧)Termの表示処理
-- Parse.hs        happyで生成した文法解析ルール
-- Environment.hs  Envを操作する関数群
-- Type.hs         PolyType,MonoTypeなどの型を定義
-- Infer.hs        変換処理
-- Shows.hs        表示処理

module Main where

import Parse
import Lex
import Term
import Type
import Environment
import InferMonad
import Infer
import  Control.Monad.Par.Scheds.Trace
import System.IO
import System.Exit
import qualified Data.Map as Map

main :: IO ()
main =  do
  l <- getContents
  case parseBinds (alexScanTokens l) of
    Left err -> die err
    Right t  -> print (inferBinds initialEnv t)

die :: String -> IO ()
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

test :: String -> IO ()
test str =
  case parseExp (alexScanTokens str) of
    Left err -> die err
    Right t  -> print (useI (error "type error") $ inferTerm initialEnv t)

inferBinds :: Env -> [(VarId,Term)] -> [(VarId,PolyType)]
inferBinds e t = runPar $ do
  -- newFullは値のあるIVarを返す
  -- unmakeEnvはEnvをMapから配列にする
  ys <- mapM (\(x,ty) -> do v <- newFull ty; return (x,v)) (unmakeEnv e)
  let topenv = Map.fromList ys
  inferTop topenv t

initialEnv :: Env
initialEnv = foldl (uncurry . extendGlobal) emptyEnv types
 where
  types = [("+",intop),("*",intop),("-",intop),("/",intop)]
  intop = All [] (intType `arrow` intType `arrow` intType)
