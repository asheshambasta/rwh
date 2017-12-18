module RecursiveContents (
  recursiveContents
  , simpleFind
) where

import Control.Monad (filterM, mapM)

import System.Directory (doesDirectoryExist, getDirectoryContents)

import System.FilePath ((</>), splitFileName)

import Glob (dirsAt)

recursiveContents :: FilePath -> IO [FilePath]
recursiveContents dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      contents <- contentsOf dir
      dirs <- dirsOnly contents
      mapM recursiveContents dirs >>= (\cs -> return (contents ++ concat cs))
    else return [dir]

contentsOf :: FilePath -> IO [FilePath]
contentsOf dir = map (dir </>) <$> getDirectoryContents dir


dirsOnly :: [FilePath] -> IO [FilePath]
dirsOnly contents = filterM doesDirectoryExist (filter notDots contents)
  where
    notDots c = fn c /= "." && fn c /= ".."
    fn = snd . splitFileName

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind f file =
  filter f <$> recursiveContents file

