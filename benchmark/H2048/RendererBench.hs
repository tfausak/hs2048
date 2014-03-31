module H2048.RendererBench (benchmarks) where

import           Criterion
import           H2048.Renderer

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "center"
        [ bench "" $ nf (center 3) "x"
        ]
    , bgroup "color"
        [ bench "Nothing" $ nf color Nothing
        ]
    , bgroup "renderBoard"
        [ bench "" $ nf renderBoard [[Nothing, Just 16]]
        ]
    , bgroup "renderGame"
        [ bench "" $ nf renderGame [[Nothing, Just 16]]
        ]
    , bgroup "renderTile"
        [ bench "Nothing" $ nf renderTile Nothing
        ]
    ]
