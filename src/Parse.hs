module Parse where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64(..))

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
  } deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

identity :: a -> Parse a
identity a = Parse ( \s -> Right (a, s) )

parse :: Parse a -> L.ByteString -> Either String a
parse parser bs =
  case runParse parser (ParseState bs 0) of
    Right (res, _) -> Right res
    Left err -> Left err
