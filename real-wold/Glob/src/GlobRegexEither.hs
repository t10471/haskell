module GlobRegexEither (globToRegex, matchesGlob) where

import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = case internal of
    Left error -> Left error
    Right val  -> Right ('^' : val ++ "$")
  where internal = globToRegex' cs

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = prepend ".*" (globToRegex' cs)
globToRegex' ('?':cs) = prepend "." (globToRegex' cs)

globToRegex' ('[':'!':c:cs) = prepend ("[^" ++ [c]) (charClass cs)
globToRegex' ('[':c:cs) = prepend ['[', c] (charClass cs)
globToRegex' ('[':_) = Left "unterminated character class"

globToRegex' (c:cs) = prepend (escape c) (globToRegex' cs)

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]"

charClass :: String -> Either GlobError String
charClass (']':cs) = prepend "]" (globToRegex' cs)
charClass (c:cs) = prepend [c] (charClass cs)
charClass _ = Left "unterminated character class"

prepend :: String -> Either GlobError String -> Either GlobError String
prepend prefix (Left error) = Left error
prepend prefix (Right str)  = Right (prefix ++ str)


matchesGlob :: FilePath -> String -> Bool
f `matchesGlob` g = case (globToRegex g) of
    Right regex -> f =~ regex
    Left err    -> False