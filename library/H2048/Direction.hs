-- | TODO
module H2048.Direction
    ( Direction (..)
    ) where

-- | TODO
data Direction
    = West
    | South
    | East
    | North
    deriving (Bounded, Enum, Eq, Show)
