-- | Functions for solving the game.
module Hs2048.AI
    ( quality
    ) where

import qualified Hs2048.Board as B

{- |
    Calculates the subject quality of a board.

    >>> quality []
    0
-}
quality :: B.Board -> Int
quality _ = 0
