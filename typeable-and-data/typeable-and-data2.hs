{-# LANGUAGE DeriveDataTypeable #-}

-- レコード構文の中身を取得するサンプル

import MyDebug
import Data.Generics.Text (gshow)
import Data.Typeable  (Typeable(..))
import Data.Data (Data(..)
                , gmapQ
                , toConstr
                , constrFields
                )
import Data.Generics.Aliases (extQ)
import Data.Maybe (fromJust)

data User = User { username  :: String
                 , usertitle :: String
                 , age       :: Int
                 } deriving (Read, Show, Typeable, Data)

introspectData :: (Data a) => a -> [(String, String)]
introspectData d = zip fields $ gmapQ gshow d
    where
        fields = constrFields $ toConstr d

main :: IO()
main = do
    let u = User {username = "John", usertitle = "jj", age = 30}
    print $ introspectData u
