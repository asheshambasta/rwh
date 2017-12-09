module Prettify where

import Numeric (showHex)
import Data.Bits ((.&.), shiftR)
import Data.Char (ord)

data Doc  = ToBeDefined
          deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

double :: Double -> Doc
double d = undefined

text :: String -> Doc
text t = undefined

-- concatenates 2 docs, same as (++) for lists
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- enclose
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

-- char
char :: Char -> Doc
char = undefined

hcat :: [Doc] -> Doc
hcat = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r  -> text r
              Nothing | mustEscape c  -> hexEscape c
                      | otherwise     -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
            <> text (replicate (4 - length h) '0')
            <> text h
            where h = showHex x ""

astral :: Int -> Doc
astral n  = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
          where
            a = (n `shiftR` 10) .&. 0x3ff
            b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000   = smallHex d
            | otherwise     = astral (d - 0x10000)
  where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item  = enclose open close
                        . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep = undefined

-- todo: understand
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


