-- | TODO
module H2048.Game
    ( randomEmptyIndex
    , randomTile
    ) where

import qualified H2048.Tile    as T
import qualified H2048.Vector  as V
import qualified System.Random as R

-- | TODO
randomEmptyIndex :: R.RandomGen r => V.Vector -> r -> (Int, r)
randomEmptyIndex v r = R.randomR (0, length (V.emptyIndexes v) - 1) r

-- | TODO
randomTile :: R.RandomGen r => r -> (T.Tile, r)
randomTile r = (Just n, r')
  where
    n = if (x :: Float) < 0.9 then 2 else 4
    (x, r') = R.random r
