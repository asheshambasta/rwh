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

type InfoP a  =   FilePath
              ->  Permissions
              ->  Maybe Integer
              ->  UTCTime
              ->  a

sizeP :: InfoP Integer
sizeP _ _ (Just size) _  = size
sizeP _ _ Nothing _      = -1

pathP :: InfoP FilePath
pathP p _ _ _  = p

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f a fp p s t = f fp p s t == a

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP abc infA b fp p i t = abc (infA fp p i t) b

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 abc ia ib fp p mi i = abc (ia fp p mi i) (ib fp p mi i) -- same as abc a b (which is  :: c)

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath fa fp _ _ _ = fa fp

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) = andP

(||?) = orP

(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP

(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(<?) = lesserP

-- set precedence for comparators
infix 4 ==?
infix 3 &&?
infix 4 >?
infix 4 <?
infix 2 ||?
