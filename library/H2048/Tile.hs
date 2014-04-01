-- | Types and functions for manipulating tiles.
module H2048.Tile
    ( Tile
    , empty
    , parse
    , rank
    , render
    , score
    ) where

{- |
    Represents a tile on the game board. Can be empty (@Nothing@) or can have a
    value (@Just n@). By convention, a tile's value is always a power of 2.
-}
type Tile = Maybe Int

{- |
    Returns the empty tile.

    >>> empty
    Nothing
-}
empty :: Tile
empty = Nothing

{- |
    Parses a string as a tile. This is the inverse of 'render'.

    >>> parse "-"
    Nothing
    >>> parse "2"
    Just 2
-}
parse :: String -> Tile
parse "-" = Nothing
parse s = Just (read s)

{- |
    Calculates the rank of a tile.

    >>> rank Nothing
    0
    >>> rank (Just 2)
    1
    >>> rank (Just 2048)
    11
-}
rank :: Tile -> Int
rank Nothing = 0
rank (Just n) = floor (logBase b n')
  where
    b = 2 :: Double
    n' = fromIntegral n

{- |
    Renders a tile as a string. This is the inverse of 'parse'.

    >>> render Nothing
    "-"
    >>> render (Just 2)
    "2"
-}
render :: Tile -> String
render Nothing = "-"
render (Just n) = show n

{- |
    Calculates the score of a tile.

    >>> score Nothing
    0
    >>> score (Just 2)
    0
    >>> score (Just 2048)
    20480
-}
score :: Tile -> Int
score Nothing = 0
score t@(Just n) = n * (rank t - 1)
