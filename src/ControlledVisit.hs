module ControlledVisit where

import System.Directory (Permissions(..), getModificationTime,
                          getPermissions, getDirectoryContents,
                          getFileSize, getModificationTime)
import System.FilePath ((</>))
import Data.Time.Clock (UTCTime)
import Control.Monad (filterM, mapM, liftM, forM)
import Control.Exception (handle, SomeException)
import Data.List (sortBy)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

traverseDir :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseDir ord fp = do
  names <- getUsefulContents fp
  infos <- mapM getInfo (map (fp </>) names ++ [fp]) -- does a deep walk before shallow walk
  liftM concat $
    forM (ord infos) $ \info ->
      if isDirectory info && infoPath info /= fp
        then traverseDir ord (infoPath info)
        else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents fp = do
            contents <- getDirectoryContents fp
            return (filter (not . (`elem` [".", ".."])) contents)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handleErr (Just <$> act)
  where
    handleErr :: SomeException -> IO (Maybe a)
    handleErr _ = return Nothing


getInfo :: FilePath -> IO Info
getInfo fp = do
      p <- maybeIO (getPermissions fp)
      s <- maybeIO (getFileSize fp)
      t <- maybeIO (getModificationTime fp)
      return (Info fp p s t)

deeperFirst = sortBy prefDir
  where
    prefDir i1 i2
      | isDirectory i1 = LT
      | otherwise = GT
