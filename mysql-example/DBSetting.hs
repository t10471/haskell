module DBSetting (
   runSQLAction
 ) where

import Database.Persist.MySQL
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Data.Word

connectInfo :: String -> Word16 -> String -> String -> String -> ConnectInfo
connectInfo ip port user password db = defaultConnectInfo {
                connectHost = ip 
               ,connectPort = port
               ,connectUser = user
               ,connectPassword = password 
               ,connectDatabase = db
}

runSQLAction :: String -> Word16 -> String -> String -> String -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction ip port user password db = runNoLoggingT . runResourceT . withMySQLConn (connectInfo ip port user password db) . runSqlConn
