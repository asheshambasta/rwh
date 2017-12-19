module BetterPredicate where

import RecursiveContents (recursiveContents)
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO(IOMode(..), hClose, hFileSize, openFile)

type Predicate  = FilePath -- path to dir. entry
                -> Permissions -- permissions
                -> Maybe Integer -- file size, Nothing if dir
                -> UTCTime -- last modified
                -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ignoreErr $
  bracket (openFile path ReadMode) hClose $ \h -> do
    s <- hFileSize h
    return (Just s)
  where ignoreErr :: SomeException -> IO (Maybe Integer)
        ignoreErr _ = return Nothing

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path =
  recursiveContents path >>= filterM check
  where check name = do
                perms <- getPermissions name
                size <- getFileSize name
                modified <- getModificationTime name
                return (p name perms size modified)

