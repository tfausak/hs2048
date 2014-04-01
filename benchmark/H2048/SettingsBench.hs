module H2048.SettingsBench (benchmarks) where

import           Criterion
import           H2048.Settings

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "height"
        [ bench "" $ nf (const height) ()
        ]
    , bgroup "maxTile"
        [ bench "" $ nf (const maxTile) ()
        ]
    , bgroup "tiles"
        [ bench "" $ nf (const tiles) ()
        ]
    , bgroup "width"
        [ bench "" $ nf (const width) ()
        ]
    ]
