{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Tue Aug 19 09:31:32 2003>
   License:    GPL
-}
{- ファイル全体が Reader モナドサンプル
runhaskell ex-reader.hs template.txt '$<#1>'
runhaskell ex-reader.hs template.txt '${language}' 'language=Haskell'
runhaskell ex-reader.hs template.txt '$"#3"'
runhaskell ex-reader.hs template.txt '$<#3>'
runhaskell ex-reader.hs template.txt '===$<no such file>==='
runhaskell ex-reader.hs template.txt '$<#2>'
runhaskell ex-reader.hs template.txt '$<#2>' 'var=dog'
runhaskell ex-reader.hs template.txt '$<#2|var=dog>'
runhaskell ex-reader.hs template.txt '$<#4|variable=cat>'
runhaskell ex-reader.hs template.txt '$<#5>' 'which=3'
runhaskell ex-reader.hs template.txt '$<#5|which=3>'
runhaskell ex-reader.hs template.txt '$<#6|which=5>'
runhaskell ex-reader.hs template.txt '$<#6|which=5,var=dog,variable=cat>'
-}

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

import Data.Maybe
import Data.List (intersperse)
import Control.Monad
import Control.Monad.Reader
import System.Environment

--              Text       Variable     Quote        Include                   Compound
data Template = T String | V Template | Q Template | I Template [Definition] | C [Template]
data Definition = D Template Template
data NamedTemplate = NT String Template

instance Show Template where
  show (T s)    = s
  show (V t)    = "${" ++ (show t) ++ "}"
  show (Q t)    = "$\"" ++ (show t) ++ "\""
  show (I t ds) = let name        = (show t)
                      definitions = concat (intersperse ", " (map show ds))
                  in case definitions of
                    []        -> "$<" ++ name ++ ">"
                    otherwise -> "$<" ++ name ++ "|" ++ definitions ++ ">"
  show (C ts)   = concatMap show ts

instance Show Definition where
  show (D t d) = (show t) ++ "=" ++ (show d)

instance Show NamedTemplate where
  show (NT n t) = "[" ++ n ++ "]" ++ (show t) ++ "[END]\n"

templateFile :: Parser [NamedTemplate]
templateFile = do 
    nts <- many namedTemplate
    eof
    return nts

namedTemplate :: Parser NamedTemplate
namedTemplate = do 
    n <- name
    t <- (template []) <?> "template"
    end
    spaces
    return (NT n t)

name :: Parser String
name = between (char '[') (char ']') (many1 (noneOf "]")) <?> "label"

end :: Parser String
end = string "[END]" <?> "[END]"

template :: [Char] -> Parser Template
template except = do 
    ts <- many1 (simpleTemplate except)
    case ts of
      [t]       -> return t
      otherwise -> return (C ts)

simpleTemplate :: [Char] -> Parser Template
simpleTemplate except =   (text except)
                      <|> (try variable)
                      <|> (try quote)
                      <|> include

dollar :: Parser Char
dollar =  try (do 
            c <- char '$'
            notFollowedBy (oneOf "{<\"")
            return c)
          <?> ""

leftBracket :: Parser Char
leftBracket = try (do 
                s <- (try end) <|> (string "[")
                case s of
                  "[END]" -> pzero
                  "["     -> return '[')
              <?> ""

textChar :: [Char] -> Parser Char
textChar except = noneOf ("$[" ++ except) <|> dollar <|> leftBracket

text :: [Char] -> Parser Template
text except = do 
                str <- many1 (textChar except)
                return (T str)
              <?> "text"

variable :: Parser Template
variable =  do 
              t <- between (string "${") (char '}') (template "}")
              return (V t)
            <?> "variable pattern"

quote :: Parser Template
quote = do 
          t <- between (string "$\"") (char '\"') (template "\"")
          return (Q t)
        <?> "quoted include pattern"

include :: Parser Template
include = between (string "$<") (char '>') includeBody
          <?> "include pattern"

includeBody :: Parser Template
includeBody = do 
    t  <- (template "|>")
    ds <- option [] definitions
    return (I t ds)

definitions :: Parser [Definition]
definitions = do 
    char '|'
    ds <- definition `sepBy1` (char ',')
    return ds

definition :: Parser Definition
definition =  do 
                t1 <- (template "=,>")
                char '='
                t2 <- (template ",>")
                return (D t1 t2)
              <?> "variable definition"

data Environment = Env  {
                          templates :: [(String,Template)]
                        , variables :: [(String,String)]
                        }

lookupVar :: String -> Environment -> Maybe String
lookupVar name env = lookup name (variables env)

lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = lookup name (templates env)

addDefs :: [(String,String)] -> Environment -> Environment
addDefs defs env = env {variables = defs ++ (variables env)}

resolveDef :: Definition -> Reader Environment (String,String)
resolveDef (D t d) = do 
    name  <- resolve t
    value <- resolve d
    return (name,value)

resolve :: Template -> Reader Environment (String)
resolve (T s)    = return s
resolve (V t)    = do 
    varName  <- resolve t
    varValue <- asks (lookupVar varName)
    return $ maybe "" id varValue
resolve (Q t)    = do 
    tmplName <- resolve t
    body     <- asks (lookupTemplate tmplName)
    return $ maybe "" show body 
resolve (I t ds) = do 
    tmplName <- resolve t
    body     <- asks (lookupTemplate tmplName)
    case body of
      Just t' -> do 
        defs <- mapM resolveDef ds
        local (addDefs defs) (resolve t')
      Nothing -> return ""
resolve (C ts)   = (liftM concat) (mapM resolve ts)

stripName :: NamedTemplate -> (String, Template)
stripName (NT n t) = (n,t)

main :: IO ()
main = do 
    args     <- getArgs
    let tmplFile = args!!0
        pattern  = args!!1
        defs     = drop 2 args
    nts      <- parseFromFile templateFile tmplFile
    case nts of
      (Left err) -> print err
      (Right _)  -> return ()
    let tmpl = parse (template []) "pattern" pattern
    case tmpl of
      (Left err) -> print err
      (Right _)  -> return ()
    let ds     = map (break (=='=')) defs
        ds'    = map (\ (x,y) -> (x,tail y)) ds
        ntl    = either (const []) id nts
        env    = Env (map stripName ntl) ds'
        t      = either (const (T "")) id tmpl
        result = runReader (resolve t) env
    putStr result
