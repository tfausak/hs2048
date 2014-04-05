module Hs2048.AIBench (benchmarks) where

import           Criterion
import           Hs2048.AI

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "bestMove"
        [ bench "" $ whnf bestMove [[Nothing, Just 2], [Just 2, Nothing]]
        ]
    , bgroup "boards"
        [ bench "[]" $ nf boards []
        ]
    , bgroup "moves"
        [ bench "[]" $ whnf moves []
        ]
    , bgroup "quality"
        [ bench "[]" $ nf quality []
        ]
    ]
