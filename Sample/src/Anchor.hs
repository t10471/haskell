module Main where

data Anchor = A { aUrl  :: String,
                   aLabel :: String }

compileAnchor :: Anchor -> String
compileAnchor ( A { aUrl = u, aLabel = l}) =
              "<a href=\"" ++ u ++ "\">" ++ l ++ "</a>"

anchorUrl :: Anchor -> String
anchorUrl (A { aUrl = u}) = u

href :: Anchor
href = A "http://exsample.com" "Home Page"

main::IO()
main = do print (aLabel href) -- "Home Pageと出力される"
          print $ compileAnchor(A "http://exsample.com" "HomePage")
          print $ anchorUrl(A "http://exsample.com" "HomePage")

