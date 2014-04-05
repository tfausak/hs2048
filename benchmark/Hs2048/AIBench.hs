module Hs2048.AIBench (benchmarks) where

import           Criterion
import           Hs2048.AI

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "boards"
        [ bench "[]" $ nf boards []
        ]
    , bgroup "moves"
        [ bench "[]" $ whnf moves []
        ]
    , bgroup "quality"
        [ bench "[]" $ nf quality []
        ]
    ]
