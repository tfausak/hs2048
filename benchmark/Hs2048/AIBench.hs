module Hs2048.AIBench (benchmarks) where

import Criterion
import Hs2048.AI

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "quality"
        [ bench "[]" $ nf quality []
        ]
    ]
