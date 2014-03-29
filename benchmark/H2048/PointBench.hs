module H2048.PointBench (benchmarks) where

import           Criterion
import           H2048.Point

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "x"
        [ bench "" $ nf x p
        ]
    , bgroup "y"
        [ bench "" $ nf y p
        ]
    ]
  where
    p = (1, 2)
