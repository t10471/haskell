{-# OPTIONS_GHC -cpp #-}

module BetterPredicate
  where

import Control.Monad(filterM)
import System.Directory(Permissions(..), getModificationTime, getPermissions)
import System.Time(ClockTime(..))
import System.FilePath(takeExtension)

#if __GLASGOW_HASKELL__ > 608
import Control.OldException(bracket, handle)
#else
import Control.Exception(bracket, handle)
#endif

import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
                 -> Permissions
                 -> Maybe Integer
                 -> ClockTime
                 -> Bool

getfilesize :: FilePath -> IO (Maybe Integer)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- setFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)

getFileSize path = handle( \_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

type InfoP a = FilePath
              -> Permissions
              -> Maybe Integer
              -> ClockTime
              -> a
pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _  -1

equalP = (Eq => a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP  = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

lift2P :: (a -> b -> c) -> InfoP a -> InfoP b -> Info c
lift2P  q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP  = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftPath :: (FilePath -> a) -> InfoP a
liftPath fw _ _ _ = fw

(==?) = equalP
(&&?) = andP
(>?)  = greaterP

infix 4 ==?
infix 3 &&?
infix 4 >?

