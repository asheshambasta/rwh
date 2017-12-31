module PasswdMap where

import Data.List
import qualified Data.Map as M
import System.IO
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (when, mapM)

data PasswdEntry = PasswdEntry {
  userName :: String,
  password :: String,
  uid :: Integer,
  gid :: Integer,
  gecos :: String,
  homeDir :: String,
  shell :: String}
  deriving (Eq, Ord)

instance Show PasswdEntry where
  show pe = printf "%s:%s:%d:%d:%s:%s:%s"
                   (userName pe) (password pe) (uid pe) (gid pe)
                   (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where
  readsPrec _ value = case split ':' value of
            [u, p, uid, gid, gecos, hd, sh] -> [(PasswdEntry u p (read uid :: Integer) (read gid :: Integer) gecos hd sh, [])]
            x -> error $ "invalid number of fields in input: " ++ show x

        where split :: (Eq a) => a -> [a] -> [[a]]
              split _ [] = []
              split a as =
                    let (pre, trail) = span (/= a) as
                    in pre : split a (tail trail)

type UIDMap = M.Map Integer PasswdEntry
type UserMap = M.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps strs = (M.fromList (aListFrom uid entries), M.fromList (aListFrom userName entries))
    where entries = map read (lines strs) :: [PasswdEntry]

aListFrom :: (Ord b) => (a -> b) -> [a] -> [(b, a)]
aListFrom f as = zip (map f as) as

start = do
    args <- getArgs

    when (length args /= 1) $ do
        putStrLn "syntax: passwdmap filename"
        exitFailure

    content <- readFile (head args)
    mainMenu $ inputToMaps content

mainMenu maps@(uidmap, usermap) = do
  putStr optionText
  hFlush stdout
  sel <- getLine
  case sel of
    "1" -> lookupUser >> mainMenu maps
    "2" -> lookupUID >> mainMenu maps
    "3" -> displayFile >> mainMenu maps
    "4" -> return ()
    _ -> putStrLn "invalid option" >> mainMenu maps
  where
    lookupUser = do
      putStrLn "username: "
      u <- getLine
      case M.lookup u usermap of
        Just up -> print up
        Nothing -> putStrLn "user not found"
    lookupUID = do
      putStrLn "UID: "
      id <- getLine
      case M.lookup (read id ) uidmap of
        Just up -> print up
        Nothing -> putStrLn "user not found"
    displayFile = mapM print (M.elems uidmap)

optionText =
 "\n options:\n"
