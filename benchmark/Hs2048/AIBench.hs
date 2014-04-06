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
    , bgroup "roughness"
        [ bench "b" $ nf roughness b
        ]
    ]
  where
    b =
        [ [Just 2048, Just 1024, Just 512, Just 256]
        , [Just 16, Just 32, Just 64, Just 128]
        , [Just 8, Just 4, Just 2, Just 2]
        , [Nothing, Nothing, Nothing, Nothing]
        ]
