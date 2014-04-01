module Hs2048.DirectionBench (benchmarks) where

import           Criterion
import           Hs2048.Direction

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "directions"
        [ bench "" $ whnf (const directions) ()
        ]
    , bgroup "render"
        [ bench "West" $ nf render West
        ]
    ]
