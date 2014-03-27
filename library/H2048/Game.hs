-- | TODO
module H2048.Game
    ( randomTile
    ) where

import qualified H2048.Tile    as T
import           System.Random (RandomGen, random)

-- | TODO
randomTile :: RandomGen g => g -> (T.Tile, g)
randomTile g = (Just n, g')
  where
    n = if (x :: Float) < 0.9 then 2 else 4
    (x, g') = random g
