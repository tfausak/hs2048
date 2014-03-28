-- | TODO
module H2048.Tile
    ( Tile
    , empty
    , parse
    , render
    , score
    ) where

import           Data.Monoid ((<>))

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
    go Nothing = "-"
    go (Just n) = show n

-- | TODO
score :: Tile -> Int
score Nothing = 0
score (Just n) = n

--

color :: Tile -> String
color t = "\ESC[" <> show (30 + go (rank t)) <> "m"
  where
    go n = if n > 6 then n + 4 else n

rank :: Tile -> Int
rank Nothing = 0
rank (Just n) = floor (logBase b (fromIntegral n))
  where
    b = 2 :: Double

reset :: String
reset = "\ESC[0m"
