module IOPlayground where

import System.IO
import Data.Char (toUpper)

main2 :: IO ()
main2 = do
          inh   <- openFile "/tmp/inp.txt" ReadMode
          outh  <- openFile "/tmp/out.txt" WriteMode
          mkUpperCase inh outh
          hClose inh
          hClose outh
          putStrLn "file uppercased, done."

mkUpperCase inh outh =
  do
    inpEOF <- hIsEOF inh
    if inpEOF
      then return ()
      else do
        inpStr <- hGetLine inh
        hPutStrLn outh (map toUpper inpStr)
        mkUpperCase inh outh

main3 :: IO ()
main3 =
  do
    inp <- readFile "/tmp/inp.txt"
    writeFile "/tmp/out.txt" (map toUpper inp)

str2Action :: String -> IO ()
str2Action = putStrLn . (++) "out: "

list2Actions :: [String] -> [IO ()]
list2Actions = map str2Action

numbers = [1..10]
strings :: [String]
strings = map show numbers

stringActs = list2Actions strings

printAll :: IO ()
printAll = runAll stringActs

runAll :: [IO ()] -> IO ()
runAll []         = return ()
runAll (cur:rem)  =
  do
    cur
    runAll rem

printStrings :: [String] -> IO ()
printStrings = mapM_ str2Action
