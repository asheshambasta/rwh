{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module JSONClass
  ( JAry(fromJAry)
  , jary
  , JObj(fromJObj)
  ) where

import Control.Arrow (second)

type JSONError = String

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue) -- was [(String, JValue)]
            | JArray (JAry JValue) -- was [JValue]
            deriving (Eq, Ord, Show)


class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "not a string"

newtype JAry a = JAry
  {
    fromJAry :: [a]
  } deriving (Eq, Ord, Show)

jary :: [a] -> JAry a
jary = JAry

newtype JObj a = JObj
  {
    fromJObj :: [(String, a)]
  } deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a))  =
  whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not an array"

mapEithers f (x:xs) =
  case mapEithers f xs of
    Left err -> Left err
    Right ys ->
      case f x of
        Left err -> Left err
        Right y -> Right (y : ys)
mapEithers _ _ = Right []

whenRight :: (b -> c) -> Either JSONError b -> Either JSONError c
whenRight _ (Left err)  = Left err
whenRight f (Right b)   = Right (f b)

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jValuesToJAry :: [JValue] -> JAry JValue
jValuesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

instance (JSON a) => JSON (JObj a) where
  toJValue  = JObject . JObj . map (second toJValue) . fromJObj

  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "not a json object"
