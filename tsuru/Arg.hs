{-# LANGUAGE DeriveDataTypeable #-}

module Arg (
      Env(..)
    , parseArgs
) where 

import System.Console.CmdArgs

data Env = Env {
              file      :: FilePath
            , isReOrder :: Bool
            } deriving (Data,Typeable,Show)

initEnv = Env {
           file      = def   &= typFile  &= argPos 0 
         , isReOrder = False &= name "r" &= explicit &= help "ReOrder by Accept Time"
         } &= program "Main"

parseArgs :: IO Env
parseArgs = cmdArgs initEnv


