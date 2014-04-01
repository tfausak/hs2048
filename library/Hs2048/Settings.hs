-- | Game settings.
module Hs2048.Settings
    ( height
    , maxTile
    , tiles
    , width
    ) where

{- |
    Returns the height of the game board.

    >>> height
    4
-}
height :: Int
height = 4

{- |
    Returns the maximum tile value.

    >>> maxTile
    2048
-}
maxTile :: Int
maxTile = 2048

{- |
    Returns the number of starting tiles.

    >>> tiles
    2
-}
tiles :: Int
tiles = 2

{- |
    Returns the width of the game board.

    >>> width
    4
-}
width :: Int
width = 4
