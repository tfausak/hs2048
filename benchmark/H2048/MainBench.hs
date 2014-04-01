module H2048.MainBench (benchmarks) where

import           Criterion
import           H2048.Main

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "direction"
        [ bench "\ESC[A" $ whnf direction "\ESC[A"
        ]
    ]
