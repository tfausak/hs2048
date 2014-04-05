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
    , -1 * roughness b
    ]

roughness :: B.Board -> Int
roughness b = boardRoughness b + boardRoughness (B.rotate b)

boardRoughness :: B.Board -> Int
boardRoughness = sum . map vectorRoughness

vectorRoughness :: V.Vector -> Int
vectorRoughness ts = sum deltas
  where
    deltas = zipWith delta ranks (tail ranks)
    delta a b = abs (a - b)
    ranks = map T.rank ts
