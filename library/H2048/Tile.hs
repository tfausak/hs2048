-- | TODO
module H2048.Tile
    ( Tile
    , empty
    , parse
    , render
    , score
    ) where

-- | TODO
type Tile = Maybe Int

-- | TODO
empty :: Tile
empty = Nothing

-- | TODO
parse :: String -> Tile
parse "-" = Nothing
parse s = Just (read s)

-- | TODO
render :: Tile -> String
render Nothing = "-"
render (Just n) = show n

-- | TODO
score :: Tile -> Int
score Nothing = 0
score (Just n) = n
