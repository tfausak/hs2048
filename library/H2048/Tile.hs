-- | TODO
module H2048.Tile
    ( Tile
    , empty
    , parse
    , render
    , score
    ) where

import Data.Monoid ((<>))

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
render t = color t <> go t <> reset
  where
    color Nothing = "\ESC[30m"
    color (Just n) = "\ESC[" <> show (30 + f (rank n)) <> "m"
    f x = if x > 6 then x + 4 else x
    rank x = floor (logBase 2 (fromIntegral x))
    go Nothing = "-"
    go (Just n) = show n
    reset = "\ESC[0m"

-- | TODO
score :: Tile -> Int
score Nothing = 0
score (Just n) = n
