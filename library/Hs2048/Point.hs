-- | Types and functions for manipulating points.
module Hs2048.Point
    ( Point
    , x
    , y
    ) where

{- |
    Represents a point on a game board. The top-left corner is (0, 0) with x
    increasing left-to-right and y increasing top-to-bottom.
-}
type Point = (Int, Int)

{- |
    Returns the x part of a point.

    >>> x (1, 2)
    1
-}
x :: Point -> Int
x = fst

{- |
    Returns the y part of a point.

    >>> y (1, 2)
    2
-}
y :: Point -> Int
y = snd
