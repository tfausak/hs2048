-- | Functions for solving the game.
module Hs2048.AI
    ( moves
    , quality
    ) where

import qualified Hs2048.Board     as B
import qualified Hs2048.Direction as D

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
quality = B.score
