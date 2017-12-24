module TreeMap where

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Show)

treeLengths :: Show a => Tree a -> Tree Int
treeLengths (Leaf a) = Leaf (length (show a))
treeLengths (Node t1 t2) = Node (treeLengths t1) (treeLengths t2)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node t1 t2) = Node (treeMap f t1) (treeMap f t2)

instance Functor Tree where
  fmap = treeMap
