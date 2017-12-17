module HighestClose where

import qualified Data.ByteString.Lazy.Char8 as L

closing = readPrice . (!!4) . L.split ','

readPrice str =
  case L.readInt str of
    Nothing             -> Nothing
    Just (dollars, rem) ->
      case L.readInt (L.tail rem) of
        Nothing       -> Nothing
        Just (c, _)   -> Just (dollars * 100 + c)

highestClosing = maximum . (Nothing:) . map closing . L.lines

highestInFile fname = highestClosing <$> L.readFile fname
