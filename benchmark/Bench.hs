module Main (main) where

import           Criterion.Main  (bgroup, defaultMain)
import qualified H2048.TileBench
import qualified H2048Bench
-- HASKELETON: import qualified New.ModuleBench

main :: IO ()
main = defaultMain
    [ bgroup "H2048" H2048Bench.benchmarks
    , bgroup "H2048.Tile" H2048.TileBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
