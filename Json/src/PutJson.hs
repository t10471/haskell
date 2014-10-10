module PutJson where

import Data.List(intercalate)
import SimpleJson

-- :! system command
renderValue :: JValue -> String

renderValue (JString s) = show s
renderValue (JNumber n) = show n
renderValue (JBool True) = "true"
renderValue (JBool False) = "false"
renderValue JNull = "null"
renderValue (JObject o) = "{" ++ pairs o ++ "}"
  where
         pairs [] = ""
         pairs ps = intercalate ", " (map renderPair ps)
         renderPair (k,v) = show k ++ ": " ++ renderValue v

renderValue (JArray a) = "[" ++ values a ++ "]"
  where
         values [] = ""
         values vs = intercalate ", " (map renderValue vs)


putJValue :: JValue -> IO()
putJValue v = putStrLn (renderValue v)

