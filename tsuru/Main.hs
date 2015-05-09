
import Market
import Arg

main :: IO ()
main = do
  e <- parseArgs 
  m <- parsePcap e
  printQuotes m e
