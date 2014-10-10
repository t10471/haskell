module ControledVisit
  where

import Control.Monad (forM, liftM)
import System.Time (ClockTime(..))
import System.Directory (Permissions(..), getDirectoryContents)
import System.FilePath ((</>))

data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Mybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Mybe ClockTime
  } deriving (Eq, Ord, Show)

traverse :: ([Info] -> Info) -> FilePath -> IO [Info]
traverse :: order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  iftM concat $ forM (order cntents) $ \info -> do
    if isDirecotory info && infoPath info /= path
      then traverse orer (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".",".."]) names)

isDirecotory :: Info -> Bool
isDirecotory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act handle(\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModifcationTime path)
  return (Info path perms size modified)

