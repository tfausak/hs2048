-- | Types and functions for manipulating directions.
module H2048.Direction
    ( Direction (..)
    , directions
    ) where

{- |
    Represents a direction that the game board can be moved in. The cardinal
    directions are used to avoid collisions with 'Left' and 'Right'.
-}
data Direction
    = West
    | South
    | East
    | North
    deriving (Bounded, Enum, Eq, Show)

{- |
    Returns all of the directions.

    >>> directions
    [West,South,East,North]
-}
directions :: [Direction]
directions = [minBound ..]
