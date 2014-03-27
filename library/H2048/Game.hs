-- | TODO
module H2048.Game
    ( addRandomTile
    , isOver
    , randomEmptyIndex
    , randomEmptyPoint
    , randomTile
    ) where

import qualified H2048.Board   as B
import qualified H2048.Point   as P
import qualified H2048.Tile    as T
import qualified H2048.Vector  as V
import qualified System.Random as R

-- | TODO
addRandomTile :: R.RandomGen r => B.Board -> r -> (B.Board, r)
addRandomTile b r = (b', r'')
  where
    b' = B.set b t p
    (t, r') = randomTile r
    (p, r'') = randomEmptyPoint b r'

-- | TODO
isOver :: B.Board -> Bool
isOver b = cantMove && haveNoEmptyPoints
  where
    cantMove = not (any (B.canMove b) [minBound ..])
    haveNoEmptyPoints = null (B.emptyPoints b)

-- | TODO
randomEmptyIndex :: R.RandomGen r => V.Vector -> r -> (Int, r)
randomEmptyIndex v r = (i, r')
  where
    i = is !! x
    (x, r') = R.randomR (0, length is - 1) r
    is = V.emptyIndexes v

-- | TODO
randomEmptyPoint :: R.RandomGen r => B.Board -> r -> (P.Point, r)
randomEmptyPoint b r = (p, r')
  where
    p = ps !! x
    (x, r') = R.randomR (0, length ps - 1) r
    ps = B.emptyPoints b

-- | TODO
randomTile :: R.RandomGen r => r -> (T.Tile, r)
randomTile r = (Just n, r')
  where
    n = if (x :: Float) < 0.9 then 2 else 4
    (x, r') = R.random r
