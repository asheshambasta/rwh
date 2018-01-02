module Parse where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Int (Int64(..))
import Data.Word (Word8)
import Data.Char (chr, isSpace, isDigit)
import PNM (Greymap(..))

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

instance Applicative Parse where
  pure = identity
  pa2b <*> pa =
    pa2b ==> \a2b ->
    pa ==> \a ->
    identity (a2b a)

instance Monad Parse where
  return = identity
  (>>=) = (==>)
  fail = bail

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

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState -- the leading fmap is the functor for the optional pair type out of L.uncons

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte -- the fmap is the function for the optional Word8 out of peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n
(==>&) :: Parse a -> Parse b -> Parse b
pa ==>& pb = pa ==> const pb

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert b err = if b then identity () else bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
        assert (L.length h == n') "end of input" ==>&
        identity h

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n pa = pa ==> \a -> (a:) <$> parseTimes (n-1) pa

