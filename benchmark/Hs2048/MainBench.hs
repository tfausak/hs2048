module Hs2048.MainBench (benchmarks) where

import           Criterion
import           Hs2048.Main

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "direction"
        [ bench "\ESC[A" $ whnf direction "\ESC[A"
        ]
    ]
