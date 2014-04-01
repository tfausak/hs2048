-- | Types and functions for manipulating directions.
module H2048.Direction
    ( Direction (..)
    , directions
    , render
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

{- |
    Renders a direction as a string.

    >>> render West
    "\8592"
-}
render :: Direction -> String
render West = "\x2190"
render South = "\x2193"
render East = "\x2192"
render North = "\x2191"
