module PrettyJSON
 (
  renderJValue
 ) where

import SimpleJSON
import Prettify (Doc, series, string, double, text, (<>))

renderJValue :: JValue -> Doc
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue JNull          = text "null"
renderJValue (JNumber n)    = double n
renderJValue (JString s)    = string s

renderJValue (JArray ary)   = series '[' ']' renderJValue ary
renderJValue (JObject obj)  = series '{' '}' field obj
  where field (name, val)   = string name
                              <> text ": "
                              <> renderJValue val

