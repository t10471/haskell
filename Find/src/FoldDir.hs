module FldDir
  where

import Control.Monad (forM, liftM)
import System.Time (ClockTime(..))
import System.Directory (Permissions(..), getDirectoryContents)
import System.FilePath ((</>))

data Iterate seed = Done { unwrap :: seed}
                    | Skip { unwrap :: seed}
                    | Continue { unwrap :: seed}
                      deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
{-
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed
    walk seed (name:names) = do
     let path' = path </> name
      info <-getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip
-}
foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk subpath seed
    walk cPath seed (name:names) = do
      let path' = cPath </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk cPath seed' names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed'' -> walk cPath (unwrap seed'') names
                | otherwise -> walk cPath seed' names
    walk _ seed _ = return (Continue seed)

atmostThreePictures :: Iterator [FilePath]
atmostThreePictures  path info
  | lenth paths == 3
    = Done paths
  | isDirectory info && takeFileName path == ".svn"
    = Skip paths
  | extension `elem` [".jpg", ".png"]
    = Continue (path:paths)
  | otherwise
    = Continue paths
  where
    extension = map tolower (takeExtension path)
    path = infoPath info

