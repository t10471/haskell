module Useful
  where
import System.FilePath(replaceExtention)
import System.Directory( doesFileExists, renameDirectory, renameFile)
import Glob(nameMatching)

renameWith :: (FilePath -> FilePath)
           -> FilePath
           -> IO FilePath
renameWith f path = do
  let path' = f path
  rename path path'
  return path'

rename :: FilePath -> FilePath -> IO ()
rename old new = do
  isFile <- doesFileExist old
  let f = if isFile then renameFile else renameDirectory
  f old new

cc2cpp =
    mapM (renameWith ( flip replaceExtension ".cpp")) =<< namesMatching "*.cc"