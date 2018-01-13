module KnightsQuest where

import Control.Monad
{- simple type synonym for a position on a chess board -}
type Pos = (Int, Int)
{- a move from p1 to p2 -}
data Move = Start Pos | Moved Move Pos
    deriving (Eq)
-- a nicer way to serialise a Move
instance Show Move where
 show (Start p) = show p
 show (Moved m p) = show m ++ " -> " ++ show p

-- gets a new starting position
newStartPos :: Move -> Pos
newStartPos (Start p)  = p
newStartPos (Moved _ p) = p

-- checks if a position is valid (can be on a chess board)
onBoard :: Pos -> Bool
onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- takes a 'move' and shows the number of times the piece has
-- been moved since the 'Start'
count :: Move -> Int
count (Start _)   = 0
count (Moved m _)  = 1 + count m

-- checks if a move has ended at a given position
endsAt :: Pos -> Move -> Bool
endsAt p (Start p1)   = p1 == p
endsAt p (Moved _ p1)  = p1 == p

-- gets a list of possible knight's moves from a given position
moves :: Move -> [Move]
moves (Start (c, r))  = fromPos (Start (c, r))
moves (Moved p (c, r)) = fromPos (Moved p (c, r))

-- gets a list of all "series" 3 moves from a given start position
three :: Move -> [Move]
three m = moves m >>= moves >>= moves

-- takes 2 positions and returns a list of moves from start -> end in exactly 3 moves of a knight
connections3 :: Pos -> Pos -> [Move]
connections3 s e = filter (endsAt e) (three (Start s))

{-connections :: Int -> Pos -> Pos -> [Move] -> [Move]
connections 0 _ e ms = filter (endsAt e) ms
connections n s e ms = fromPos (Start s) >>= (\m -> connections (n -1) (newStartPos m) e (m:ms))-}

-- proposes all valid positions a knight can reach from a given 'previous' move.
fromPos :: Move -> [Move]
fromPos move =
  let (c, r) = newStartPos move
  in do
    guard (onBoard (c, r)) -- fail if start pos is not on board to begin with
    m <-
      [ (c + 2, r - 1)
      , (c + 2, r + 1)
      , (c - 2, r - 1)
      , (c - 2, r + 1)
      , (c + 1, r - 2)
      , (c + 1, r + 2)
      , (c - 1, r - 2)
      , (c - 1, r + 2)
      ] -- all possible moves
    guard (onBoard m) -- fail if move is off board
    return (Moved move m)
