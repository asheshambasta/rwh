module Barcode where

import Data.Function (on)
import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as M
import Parse

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

rightList  = map complement <$> leftOddList
             where complement '1' = '0'
                   complement '0' = '1'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs
    where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
              where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f a0 bs = go a0 (indices bs)
    where go a0 (j:js) = let a0' = f a0 (bs ! j)
                         in a0 `seq` go a0' js
          go a0 []     = a0

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f as = foldA f first as
  where
    first = as ! fst (bounds as)

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
    outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
  where (left, right) = splitAt 5 rest
        lefties = zipWith leftEncode (parityCodes ! first) left
        righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

outerGuard = "101"
centerGuard = "01010"

{-
  the nomenclature used below is baffling (p. 278).
  The authors describe a "pixel" to be represented as 3 bytes. Then they go on
  to call a byte a pixel and a pixel an RGB. Pixmap also becomes a 2d array of RGB's and not 'Pixels'.
  This is one of the few instances in the book where the naming is really off the mark
  and very confusing.
-}
type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB

parseRawPPM :: Parse Pixmap
parseRawPPM =
    parseWhileWith w2c (/= '\n') ==> \header -> skipSpaces ==>&
    assert (header == "P6") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxValue ->
    assert (maxValue == 255) "max value out of spec" ==>&
    parseByte ==>&
    parseTimes (width * height) parseRGB ==> \pxs ->
    identity (listArray ( (0,0), (width - 1, height - 1) ) pxs)

parseRGB :: Parse RGB
parseRGB = parseByte ==> \p1 ->
           parseByte ==> \p2 ->
           parseByte ==> \p3 -> identity (p1, p2, p3)

luminance :: RGB -> Pixel
luminance (r, g, b) = round (r' * 0.3 + g' * 0.59 + b' * 0.11)
    where r' = fromIntegral r
          g' = fromIntegral g
          b' = fromIntegral b

data Bit = Zero | One
           deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold th arr = binary <$> arr
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot     = round $ least + (greatest - least) * th
          least     = fromIntegral $ choose (<) arr
          greatest  = fromIntegral $ choose (>) arr
          choose f  = foldA1 $ \x y -> if f x y then x else y

type Run = Int
type RunLength a = [(Run, a)]

runLength :: (Eq a) => [a] -> RunLength a
runLength = map rle . group
            where rle xs = (length xs, head xs)

runLengths :: (Eq a) => [a] -> [Run]
runLengths = map fst . runLength

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne runs = map divide runs
    where divide d  = fromIntegral d / divisor
          divisor = fromIntegral (sum runs)

type ScoreTable = [[Score]]

asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
distance s1s s2s = sum  . map abs $ zipWith (-) s1s s2s

bestScores :: ScoreTable -> [Run] -> [(Score, Int)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]

data Parity a = Odd a | Even a | None a
              deriving (Show)

fromParity :: Parity a -> a
fromParity (Odd a)  = a
fromParity (Even a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Odd a)  = Odd (f a)
parityMap f (Even a) = Even (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
  fmap = parityMap

compareWithoutParity :: (Ord a) => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity
