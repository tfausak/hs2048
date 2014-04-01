module Hs2048.BoardBench (benchmarks) where

import           Criterion
import           Hs2048.Board
import qualified Hs2048.Direction as D

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "canMove"
        [ bench "North" $ nf (canMove b) D.North
        ]
    , bgroup "canShift"
        [ bench "" $ nf canShift b
        ]
    , bgroup "empty"
        [ bench "4 4" $ nf (empty 4) 4
        ]
    , bgroup "emptyPoints"
        [ bench "" $ nf emptyPoints b
        ]
    , bgroup "move"
        [ bench "North" $ nf (move b) D.North
        ]
    , bgroup "parse"
        [ bench "" $ nf parse (render b)
        ]
    , bgroup "render"
        [ bench "" $ nf render b
        ]
    , bgroup "rotate"
        [ bench "" $ nf rotate b
        ]
    , bgroup "rotateFrom"
        [ bench "South" $ nf (rotateFrom b) D.South
        ]
    , bgroup "rotateTo"
        [ bench "North" $ nf (rotateTo b) D.North
        ]
    , bgroup "score"
        [ bench "" $ nf score b
        ]
    , bgroup "set"
        [ bench "" $ nf (set b (Just 2)) (3, 3)
        ]
    , bgroup "shift"
        [ bench "" $ nf shift b
        ]
    ]
  where
    b = [ [Nothing, Just 2, Nothing, Just 2]
        , [Just 2, Nothing, Just 2, Nothing]
        , [Nothing, Just 4, Nothing, Just 4]
        , [Just 4, Nothing, Just 4, Nothing]
        ]
