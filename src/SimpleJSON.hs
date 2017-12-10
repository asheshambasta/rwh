module SimpleJSON
 (
  JValue(..),
  getString,
  getDouble,
  getInt,
  getBool,
  getObject,
  getArray
 ) where

-- defines the JValue type

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray[JValue]
            deriving (Eq, Ord, Show)

getString (JString s) = Just s
getString _           = Nothing

getDouble (JNumber d) = Just d
getDouble _           = Nothing

getInt (JNumber d)    = Just (truncate d)
getInt _              = Nothing

getBool (JBool b)     = Just b
getBool _             = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a)   = Just a
getArray _            = Nothing

isNull v              = v == JNull


