module Main  where

import SimpleJson
import PrettyJson

-- main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
-- main  = let value =  renderJValue(JObject[("f", JNumber 1), ("q", JBool True)])

main  = print(  renderJValue(JObject[("f", JNumber 1), ("q", JBool True)]))

