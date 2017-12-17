module Main where

import Lib
import SimpleJSON
import PutJSON

import System.IO
import Data.Char (toUpper)

name2rep name = "Hi " ++ name ++ ". Your name has " ++ show (length name) ++ " characters."

main :: IO ()
main =  putStrLn "Hello! What's your name?" >>
        getLine >>=
        (putStrLn . name2rep)



