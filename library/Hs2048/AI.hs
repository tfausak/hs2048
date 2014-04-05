-- | Functions for solving the game.
module Hs2048.AI
    ( bestMove
    , boards
    , moves
    , quality
    ) where

import           Data.List        (genericLength, group, maximumBy, sort)
import           Data.Maybe       (catMaybes, isJust)
import           Data.Ord         (comparing)
import qualified Hs2048.Board     as B
import qualified Hs2048.Direction as D
import qualified Hs2048.Tile as T
import qualified Hs2048.Vector as V
import qualified Hs2048.Point as P

{- |
    Calculates the average value of a list.

    >>> average [1, 2, 3]
    2.0
-}
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

{- |
    Determines the best direction to move a board in.

    >>> bestMove [[Nothing, Just 2], [Nothing, Just 2]]
    South
-}
bestMove :: B.Board -> D.Direction
bestMove b = fst (maximumBy go bs)
  where
    go = comparing ((average :: [Int] -> Float) . fmap quality . boards . snd)
    bs = reverse (moves b)

{- |
    Generates all possible next states from a board.

    >>> boards []
    []
-}
boards :: B.Board -> [B.Board]
boards b = ps >>= go
  where
    ps = B.emptyPoints b
    go p = [B.set b t p | t <- [Just 2, Just 4]]

{- |
    Detects the duplicate tiles on a board and returns them in groups.

    >>> duplicates [[Nothing, Just 2], [Just 2, Nothing]]
    [[2,2]]
-}
duplicates :: B.Board -> [[Int]]
duplicates = group . sort . (=<<) catMaybes

{- |
    Determines which directions a board can be moved in.

    >>> moves []
    []
-}
moves :: B.Board -> [(D.Direction, B.Board)]
moves b = fmap go ds
  where
    go d = (d, B.move b d)
    ds = filter (B.canMove b) D.directions

{- |
    Calculates the subject quality of a board.

    >>> quality []
    0
-}
quality :: B.Board -> Int
quality b = sum
    [ 0
    , 1 * B.score b
    , 1 * length (moves b)
    , 1 * length (B.emptyPoints b)
    , -1 * length (duplicates b)
    , -1 * smoothness b
    ]

--

neighbors :: B.Board -> P.Point -> [P.Point]
neighbors b (x, y) = filter (inBounds b)
    [ (x - 1, y    )
    , (x + 1, y    )
    , (x    , y - 1)
    , (x    , y + 1)
    ]

inBounds :: B.Board -> P.Point -> Bool
inBounds b (x, y) =
    x >= 0 &&
    y >= 0 &&
    y < height &&
    x < width
  where
    height = length b
    width = if null b then 0 else length (head b)

difference :: T.Tile -> T.Tile -> Int
difference a b = T.rank a - T.rank b

get :: B.Board -> P.Point -> T.Tile
get b (x, y) = b !! y !! x

points :: B.Board -> [P.Point]
points b =
    [ (x, y)
    | x <- [0 .. width - 1]
    , y <- [0 .. height - 1]
    ]
  where
    height = length b
    width = if null b then 0 else length (head b)

edges :: B.Board -> [(P.Point, P.Point)]
edges b = concatMap (\ p -> zip (repeat p) (neighbors b p)) (points b)

weight :: B.Board -> (P.Point, P.Point) -> Int
weight b (p1, p2) = abs (difference (get b p1) (get b p2))

smoothness :: B.Board -> Int
smoothness b = sum (map (weight b) (edges b))
