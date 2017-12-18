module BetterPredicate where

import RecursiveContents (recursiveContents)
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO(IOMode(..), hClose, hFileSize, openFile)

type Predicate  = FilePath -- path to dir. entry
                -> Permissions -- permissions
                -> Maybe Integer -- file size, Nothing if dir
                -> ClockTime -- last modified
                -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize = undefined

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind = undefined

