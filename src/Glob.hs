module Glob (namesMatching, dirsAt) where

import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)

import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle, SomeException)

import Control.Monad (forM, filterM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

isPatternRecursive :: String -> Bool
isPatternRecursive ('*':'*':_)  = True
isPatternRecursive _            = False

namesMatching pat
  | not (isPattern pat) =
    do
      exists <- doesNameExist pat
      return [pat | exists] -- eqv. to ```if exists then [pat] else []```
  | otherwise =
    case splitFileName pat of
      ("", baseName) ->
        do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <-
          if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
        let listDir =
              if isPattern baseName
                then listMatches
                else listPlain
        pathNames <-
          forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
        return (concat pathNames)

doesNameExist name =
  do
    f <- doesFileExist name
    if f then return True else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName -- ""
      then getCurrentDirectory
      else return dirName
  handle ignoreErr $ do
    names <- getDirectoryContents dirName'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    let currentMatches = map (dirName' </>) (filter (`matchesGlob` pat) names')
    if isRecursive
      then do
        dirsHere <- dirsAt dirName' -- get directories in cwd
        ((++) currentMatches . concat) <$> forM dirsHere (`listMatches` pat)
      else return currentMatches
  where
    ignoreErr :: SomeException -> IO [String]
    ignoreErr _ = return []
    isRecursive = isPatternRecursive pat

isHidden ('.':_)  = True
isHidden _        = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  return [baseName | exists]

dirsAt :: FilePath -> IO [FilePath]
dirsAt dir =
  map (dir </>) . filter notDots <$> (getDirectoryContents dir >>= filterM (doesDirectoryExist . (</>) dir))
  where
    notDots r = not (r == "." || r == "..")
