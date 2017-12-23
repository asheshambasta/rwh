module PNM where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
  greyWidth :: Int,
  greyHeight :: Int,
  greyMax :: Int,
  greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  matchHeader (L8.pack "P5") s  >>?
  \bs -> skipSpace2nd ((), bs)  >>?
  (getNatural . snd)            >>?
  skipSpace2nd                  >>?
  \(w, bs) -> getNatural bs     >>?
  skipSpace2nd                  >>?
  \(h, bs) -> getNatural bs     >>?
  \(mx, bs) -> getBytes 1 bs    >>?
  (getBytes (w*h) . snd)        >>?
  \(bitmap, rest) -> Just (Greymap w h mx bitmap, rest)


matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise = Nothing

getNatural :: L.ByteString -> Maybe (Int, L.ByteString)
getNatural bs = case L8.readInt bs of
                Nothing   -> Nothing
                Just (i, res)  -> if i < 0 then Nothing else Just (i, res)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n bs = let num = fromIntegral n
                    both@(prefix, _) = L.splitAt num bs
                in  if L.length prefix < num
                    then Nothing
                    else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>?) (Just a) f  = f a
(>>?) Nothing _   = Nothing

skipSpace2nd :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace2nd (a, bs) = Just (a, L8.dropWhile isSpace bs)

