-- | Functions for solving the game.
module Hs2048.AI
    ( bestMove
    , boards
    , moves
    , quality
    , roughness
    ) where

import           Data.List        (genericLength, maximumBy)
import           Data.Maybe       (catMaybes)
import           Data.Ord         (comparing)
import qualified Hs2048.Board     as B
import qualified Hs2048.Direction as D
import qualified Hs2048.Vector    as V

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
    , -1 * roughness b
    ]

{- |
    Calculates the roughness of a board. A rough board has lots of different
    tiles next to each other. A smooth board has lot of the same tiles next to
    each other. Smooth boards are generally better.

    >>> roughness [[Just 2, Just 4]]
    1
    >>> roughness [[Just 2, Just 2]]
    0

    Blank tiles are ignored for the purposes of calculating roughness.

    >>> roughness [[Just 2, Nothing, Just 4]]
    1
-}
roughness :: B.Board -> Int
roughness b = boardRoughness b + boardRoughness (B.rotate b)

--

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

boardRoughness :: B.Board -> Int
boardRoughness = sum . fmap vectorRoughness

vectorRoughness :: V.Vector -> Int
vectorRoughness v = length (filter id (zipWith (/=) ts (tail ts)))
  where
    ts = catMaybes v
