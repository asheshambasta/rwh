module Main where

import Lib
import SimpleJSON
import PutJSON

main :: IO ()
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False), ("baz", JNull), ("foo2", JString "foobar")])
