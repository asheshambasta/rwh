module Parse where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64(..))
import Data.Word (Word8)
import Data.Char (chr)

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
  } deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

instance Functor Parse where
  fmap f p1 =
    p1 ==> \s -> identity (f s)

identity :: a -> Parse a
identity a = Parse ( \s -> Right (a, s) )

parse :: Parse a -> L.ByteString -> Either String a
parse parser bs =
  case runParse parser (ParseState bs 0) of
    Right (res, _) -> Right res
    Left err -> Left err

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset init newoff = init { offset = newoff }

parseByte :: Parse Word8
parseByte =
  getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte, remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse ( \s -> Right (s, s) )

putState :: ParseState -> Parse ()
putState s = Parse ( \_ -> Right ((), s) )

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
         case runParse firstParser initState of
            Left err -> Left err
            Right (res1, newState) -> runParse (secondParser res1) newState

parseChar :: Parse Char
parseChar = chr . fromIntegral <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState -- the leading fmap is the functor for the optional pair type out of L.uncons

peekChar :: Parse (Maybe Char)
peekChar = fmap (chr . fromIntegral) <$> peekByte -- the fmap is the function for the optional Word8 out of peekByte
