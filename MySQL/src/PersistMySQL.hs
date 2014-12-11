{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import DBSetting
import Data.Word
import System.Environment

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
    name String
    age Int Maybe
BlogPost
    title String
    authorId PersonId
|]
 
main :: IO ()
main = do
  ip <- getEnv "MYSQL_PORT_3306_TCP_ADDR"
  port <- getEnv "MYSQL_PORT_3306_TCP_PORT"
  user <- getEnv "MYSQL_ENV_MYSQL_USER"
  password <- getEnv "MYSQL_ENV_MYSQL_PASSWORD"
  db <- getEnv "MYSQL_ENV_MYSQL_DATABASE"
  registerData ip (read port :: Word16) user password db
  putStrLn "OK Complete!"
 
registerData :: String -> Word16 -> String -> String -> String -> IO ()
registerData ip port user password db = runSQLAction ip port user password db $ do
       runMigration migrateAll
       takeId <- insert $ Person "Take Ishii" $ Just 40
       asaId <- insert $ Person "Kazu Asaka" $ Just 41
       _ <- insert $ BlogPost "今日も酒を飲みすぎました" takeId
       _ <- insert $ BlogPost "今日は法事が入っています" asaId
       return ()
 
