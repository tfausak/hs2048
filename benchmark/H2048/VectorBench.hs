module H2048.VectorBench (benchmarks) where

import           Criterion
import           H2048.Vector

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "canShift"
        [ bench "v1" $ nf canShift v1
        , bench "v2" $ nf canShift v2
        ]
    , bgroup "empty"
        [ bench "4" $ nf empty 4
        ]
    , bgroup "emptyIndexes"
        [ bench "v1" $ nf emptyIndexes v1
        , bench "v2" $ nf emptyIndexes v2
        ]
    , bgroup "parse"
        [ bench "- - - -" $ nf parse "- - - -"
        , bench "2 2 2 2" $ nf parse "2 2 2 2"
        ]
    , bgroup "render"
        [ bench "v1" $ nf render v1
        , bench "v2" $ nf render v2
        ]
    , bgroup "score"
        [ bench "v1" $ nf score v1
        , bench "v2" $ nf score v2
        ]
    , bgroup "set"
        [
        ]
    , bgroup "shift"
        [ bench "v1" $ nf shift v1
        , bench "v2" $ nf shift v2
        ]
    ]
  where
    v1 = replicate 4 Nothing
    v2 = replicate 4 (Just 2)
