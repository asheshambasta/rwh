{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module JSONClass where

import SimpleJSON (JValue(..))

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON String where
  toJValue = JString
  fromJValue (JString s)  = Right s
  fromJValue _            = Left "not a string"
