module Glob
  (
      namesMatching
  ) where

import System.Directory(doesDirectoryExists, doesFileExists,
                           getCurrentDirectory, getDirecotryContents)
import System.FilePath(dropTrailingPathSeparator, splitFileName, (</>))
import Control.OldException (handle)
import Control.Monad(forM)
import GlobRegex(matchesGlob)


isPattern :: String -> Bool
isPattenr = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    case splitFileName pat of
      ("", basName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                    then namesMatching (dorpTrailingPathSeparator dirName)
                    else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                          baseNames <- listDir dr baseName
                          return (map (dir </>) baseNames)
          retrun (concat pathNames)


doesNameExist :: FilePath -> IO Bool
doesNmaeExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then retrun true
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
  handle (const (return [])) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                    then ilter isHidden names
                    else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
              then doesDirectoryExists dirName
              else doesNameExist (dirName </> baseNmae)
  return(if exists then [baseNmae] else [])
