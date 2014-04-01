module Hs2048.TileBench (benchmarks) where

import           Criterion
import           Hs2048.Tile

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "empty"
        [ bench "" $ nf (const empty) ()
        ]
    , bgroup "parse"
        [ bench "-" $ nf parse "-"
        , bench "2" $ nf parse "2"
        ]
    , bgroup "rank"
        [ bench "Nothing" $ nf rank Nothing
        , bench "Just 2" $ nf rank (Just 2)
        , bench "Just 2048" $ nf rank (Just 2048)
        ]
    , bgroup "render"
        [ bench "Nothing" $ nf render Nothing
        , bench "Just 2" $ nf render (Just 2)
        ]
    , bgroup "score"
        [ bench "Nothing" $ nf score Nothing
        , bench "Just 2" $ nf score (Just 2)
        , bench "Just 2048" $ nf score (Just 2048)
        ]
    ]
