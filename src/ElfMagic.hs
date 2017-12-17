module ElfMagic where

import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic cont = L.take 4 cont == elfMagic
                  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
{-
the following function is equivalent to:
```
L.readFile fname >>= return . hasElfMagic
```
and is less verbose
(suggested by haskell plugin)
-}
isElfFile fname = hasElfMagic <$> L.readFile fname

