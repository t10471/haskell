{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}

import Control.Eff
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy
import Data.Typeable (Typeable)
import Control.Monad

type Task = String
newtype TaskList = TaskList { todo :: [Task] }
  deriving (Eq, Show, Typeable)

-- State版
-- タスクリストをクリアする
resetTodo :: (Member (State TaskList) r) => Eff r ()
resetTodo = put $ TaskList ["EOF"]

-- Reader版
nextTask :: (Member (Reader TaskList) r) => Eff r Task
nextTask = do
  TaskList u <- ask
  return $ head u

-- State版
nextTask' :: (Member (State TaskList) r) => Eff r Task
nextTask' = do
  TaskList u <- get
  return $ head u

-- ここではクロージャーっぽいことをしている
updateHead :: (Member (State TaskList) r) => (Task -> Task) -> Eff r ()
updateHead f = do
  TaskList ts <- get
  put $ TaskList $ go f ts
  where
    go :: (s -> s) -> [s] -> [s]
    go _ []     = []
    go f (a:as) = f a:as

example1 = do
  let w = TaskList ["todo1", "todo2", "todo3", "EOF"]
  putStrLn $ run $ runReader nextTask w
  -- output: todo1
  print $ run $ runState w nextTask'
  -- output: (TaskList {todo = ["todo1","todo2","todo3","EOF"]},"todo1")
  putStrLn $ run $ evalState w nextTask'
  -- output: todo1
  print $ run $ runState w resetTodo
  -- output: (TaskList {todo = ["EOF"]},())
  putStrLn $ run . runReader nextTask
           $ run . execState w
           $ updateHead (++ ": 1st task")
  -- output: todo1: 1st task

-- Taskを1つだけ処理する
doTask :: (Member (Writer String) r, Member (State TaskList) r) => Eff r ()
doTask = do
  TaskList ts <- get
  -- 最後はEOFがはいっている
  when (length ts >= 2) $ do
    put $ TaskList $ tail ts
    tell $ "task was done -> " ++ (head ts) ++ ".\n"

-- Taskを全て処理する
doTaskAll :: (Member (Writer String) r, Member (State TaskList) r) => Eff r ()
doTaskAll = do
  doTask
  TaskList ts <- get
  -- 最後はEOFがはいっている
  when (length ts >= 2) $ doTaskAll

example2 = do
  let w = TaskList ["todo1", "todo2", "todo3", "EOF"]
  print $ run $ runWriter (++) "" $ execState w doTask
  -- output: ("task was done -> todo1.\n",TaskList {todo = ["todo2","todo3","EOF"]})
  print $ run $ runWriter (++) "" $ execState w doTaskAll
  -- output: ("task was done -> todo1.\ntask was done -> todo2.\ntask was done -> todo3.\n",TaskList {todo = ["EOF"]})
  print $ run $ execState w $ runWriter (++) "" $ doTaskAll
  -- output: TaskList {todo = ["EOF"]}

main = do
  example1
  example2
