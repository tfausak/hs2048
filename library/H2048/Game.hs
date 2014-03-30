-- | Functions for playing the game.
module H2048.Game
    ( addRandomTile
    , addRandomTiles
    , hasWon
    , isOver
    , new
    , randomEmptyIndex
    , randomEmptyPoint
    , randomTile
    ) where

import           Data.Maybe      (fromJust)
import qualified H2048.Board     as B
import qualified H2048.Direction as D
import qualified H2048.Point     as P
import qualified H2048.Settings  as S
import qualified H2048.Tile      as T
import qualified H2048.Vector    as V
import qualified System.Random   as R

{- |
    Adds a random tile to the board.

    >>> addRandomTile [[Nothing], [Nothing]] (R.mkStdGen 0)
    ([[Nothing],[Just 2]],1346387765 2103410263)
-}
addRandomTile :: R.RandomGen r => B.Board -> r -> (B.Board, r)
addRandomTile b r = case p of
    Nothing -> (b, r)
    _ -> (b', r'')
  where
    b' = B.set b t (fromJust p)
    (p, r') = randomEmptyPoint b r
    (t, r'') = randomTile r'

{- |
    Adds some random tiles to the board.

    >>> addRandomTiles 2 [[Nothing], [Nothing]] (R.mkStdGen 0)
    ([[Just 2],[Just 2]],2127568003 1780294415)
-}
addRandomTiles :: R.RandomGen r => Int -> B.Board -> r -> (B.Board, r)
addRandomTiles 0 b r = (b, r)
addRandomTiles n b r = addRandomTiles (n - 1) b' r'
  where
    (b', r') = addRandomTile b r

{- |
    Determines if the game has been won. See 'H2048.Settings.maxTile'.

    >>> hasWon [[Just 2048]]
    True
-}
hasWon :: B.Board -> Bool
hasWon = any (any (maybe False (>= S.maxTile)))

{- |
    Determines if the game is over. The game is over if there are no available
    moves and no empty points.

    >>> isOver [[Just 2]]
    True
-}
isOver :: B.Board -> Bool
isOver b = cantMove && haveNoEmptyPoints
  where
    cantMove = not (any (B.canMove b) D.directions)
    haveNoEmptyPoints = null (B.emptyPoints b)

{- |
    Creates a new game by making an empty board and adding some random tiles to
    it. See 'H2048.Settings.width', 'H2048.Settings.height', and
    'H2048.Settings.tiles'.

    >>> new (R.mkStdGen 0)
    ([[Just 2,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing],[Nothing,Just 2,Nothing,Nothing]],2127568003 1780294415)
-}
new :: R.RandomGen r => r -> (B.Board, r)
new = addRandomTiles S.tiles (B.empty S.width S.height)

{- |
    Selects an empty index at random from a vector.

    >>> randomEmptyIndex [Nothing, Nothing] (R.mkStdGen 0)
    (Just 1,40014 40692)
-}
randomEmptyIndex :: R.RandomGen r => V.Vector -> r -> (Maybe Int, r)
randomEmptyIndex v r = if null is then (Nothing, r) else (Just i, r')
  where
    i = is !! x
    (x, r') = R.randomR (0, length is - 1) r
    is = V.emptyIndexes v

{- |
    Selects an empty point at random from a board.

    >>> randomEmptyPoint [[Nothing],[Nothing]] (R.mkStdGen 0)
    (Just (1,0),40014 40692)
-}
randomEmptyPoint :: R.RandomGen r => B.Board -> r -> (Maybe P.Point, r)
randomEmptyPoint b r = if null ps then (Nothing, r) else (Just p, r')
  where
    p = ps !! x
    (x, r') = R.randomR (0, length ps - 1) r
    ps = B.emptyPoints b

{- |
    Creates a random tile.

    >>> randomTile (R.mkStdGen 0)
    (Just 2,1601120196 1655838864)
-}
randomTile :: R.RandomGen r => r -> (T.Tile, r)
randomTile r = (Just n, r')
  where
    n = if (x :: Float) < 0.9 then 2 else 4
    (x, r') = R.random r
