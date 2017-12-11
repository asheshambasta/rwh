module EqClasses where

data Colour = Red
            | Blue
            | Green

class BasicEq a where
  isEqual :: a -> a -> a

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _ _         = False

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool

class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

instance BasicEq3 Colour where
  isEqual3 Red Red      = True
  isEqual3 Blue Blue    = True
  isEqual3 Green Green  = True
  isEqual3 _ _          = False

instance Show Colour where
  show Red    = "Red"
  show Blue   = "Blue"
  show Green  = "Green"

instance Read Colour where
  readsPrec _ value =
      tryParse [ ("Red", Red), ("Blue", Blue), ("Green", Green) ]
    where
      tryParse []                                 = []
      tryParse ((attempt, res):xs)
        | take (length attempt) cleanVal == attempt  = [(res, drop (length attempt) cleanVal)]
        | otherwise                               = tryParse xs
      cleanVal = dropWhile (' ' ==) value

data CannotShow = CannotShow
                deriving (Show)

newtype CannotDeriveShow = CannotDeriveShow CannotShow
                         deriving (Show)

data OK = OK

instance Show OK where
  show _ = "OK"

newtype ThisWorks = ThisWorks OK
                  deriving (Show)
