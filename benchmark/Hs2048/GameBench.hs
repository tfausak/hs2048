module Hs2048.GameBench (benchmarks) where

import           Criterion
import           Hs2048.Board  (empty)
import           Hs2048.Game
import           System.Random (mkStdGen)

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "addRandomTile"
        [ bench "" $ whnf (addRandomTile b) r
        ]
    , bgroup "addRandomTiles"
        [ bench "" $ whnf (addRandomTiles 2 b) r
        ]
    , bgroup "hasWon"
        [ bench "" $ nf hasWon b
        ]
    , bgroup "isOver"
        [ bench "" $ nf isOver b
        ]
    , bgroup "new"
        [ bench "" $ whnf new r
        ]
    , bgroup "randomEmptyIndex"
        [ bench "" $ whnf (randomEmptyIndex (head b)) r
        ]
    , bgroup "randomEmptyPoint"
        [ bench "" $ whnf (randomEmptyPoint b) r
        ]
    , bgroup "randomTile"
        [ bench "" $ whnf randomTile r
        ]
    ]
  where
    b = empty 4 4
    r = mkStdGen 0
